#' ---
#' title: "Convert Irregular Hydrus-2D Mesh Data to Regular Grid"
#' author: "Samuel Araya"
#' date: '`r format(Sys.Date(), "%B %d, %Y")`'
#' output:
#'    html_document:
#'      toc: true
#'      keep_md: true
#' ---
# /*
## RENDER CODE:
rm(list=ls())
library(knitr)
opts_chunk$set(tidy=TRUE, warning=FALSE, message=FALSE)
library(kableExtra)
rmarkdown::render(input = "H2D_Mesh_02_Make_Grid_Uniform.R", output_dir = "Reports")
#
# */

#+ setup, warning=FALSE, message=FALSE
## Libraries
library(tidyverse)
library(lubridate)
library(janitor)
library(broom)
library(ggpubr)
library(readxl)
library(viridis) # color for ggplot
library(scales) # to access break formatting functions for ggplot
library(ggExtra)
library(gridExtra)
library(mlr)
library(akima) # Interpolation package
library(doParallel) # Parallel computation package (also loads: foreach, iterators and parallel)
## Data Directories
wDir <- getwd()
parent_dir = file.path(wDir, "Data_Raw" ,"Hydrus_Simulations_Out")
dataDir <- file.path(wDir,"Data_Raw")
pdataDir <- file.path(wDir, "Data_Processed")
outDir <- file.path(wDir,"Plot")
### Custom Functions
source("functions_custom.R")
## Function to save plots as pdf and png at one time:
save_plot <- function(myplot, myfilename, width, height,
                      mypath = outDir){
  ggsave(plot = myplot, width = width, height = height,
         filename = file.path(mypath, paste0(myfilename,".pdf")))
  
  ggsave(plot = myplot, width = width, height = height,
         filename = file.path(mypath, paste0(myfilename,".png")))
  
}

#' ## Read in Mesh Data
#'  Supply a 'look-up' table for variables
### TREATMENT (RE-)NAME AND LEVELS
notill_levels =  c("ST-NO", "ST-CC", "CT-NO", "CT-CC")
notill_labels = c("ST-NO", "ST-CC", "NT-NO", "NT-CC")
## Import look up table
plot_lookup = read_csv("Plot_Lookup.csv", 
                       col_types = cols(
                         Plot_ID = readr::col_double(),
                         Winter_Cover = col_character(),
                         Tillage_Type = col_character(),
                         Replicate = readr::col_factor()
                       ) )

#' Read water content mesh tables
th.dt = read_rds(path = file.path(pdataDir ,"Mesh_th.rds") ) 
#' Subset data to only the last irrigation. 
#' The final irrigation starts on hour 2357 and ends on: 2364
th.dt = th.dt %>% 
  ## 0 at time of START of final irrigation = week 15
  ## filter between START of irrigation and 4 days past
  dplyr::filter(Time >= 2357 & Time < (2364 + 96)) %>%  # (3360-1007 + 11)) %>%
  dplyr::left_join(plot_lookup, by = "Plot_ID") %>% 
  dplyr::mutate(Trt = factor(paste(Tillage_Type, Winter_Cover, sep = "-"), 
                             levels =  notill_levels,
                             labels = notill_labels, ordered = TRUE))

#' Aggregate (summarize) table as mean by treatment
th.dt = th.dt %>% 
  group_by(Time, Trt, x, y) %>%
  summarise(th_mean = mean(h), n = n())

#' Read suction mesh tables and summarize similarly
h.dt = read_rds(path = file.path(pdataDir ,"Mesh_h.rds") ) 
## Subset after last irrigation
h.dt = h.dt %>% 
  ## 0 at time of START of final irrigation = week 15
  ## filter between START of irrigation and 4 days past
  dplyr::filter(Time >= 2357 & Time < (2364 + 96)) %>%  # (3360-1007 + 11)) %>%
  dplyr::left_join(plot_lookup, by = "Plot_ID") %>% 
  dplyr::mutate(Trt = factor(paste(Tillage_Type, Winter_Cover, sep = "-"), 
                             levels =  notill_levels,
                             labels = notill_labels, ordered = TRUE))
#' Aggregate (summarize) by treatment
h.dt = h.dt %>% 
  group_by(Time, Trt, x, y) %>%
  summarise(h_mean = mean(h), n = n())

#' ## Merge `theta` and `h` Into on Table and Save
trt.mesh.dt = th.dt %>% 
  left_join(h.dt) %>% 
  # Create new time that starts from 0
  dplyr::mutate(Time2 = Time - 2357)

save_table(trt.mesh.dt, pdataDir ,"Mesh_Mean_th_h_Treatment")

rm(h.dt, th.dt)

#' ## Interpolate Mesh into a Regular Mesh
#' Interpolate one point every 0.5 centimeter. i.e. 18 by 100 points.
#' 
#' Create a treatment and time grid to apply interpolation function to all times
#' and treatments
#' 
trts = unique(trt.mesh.dt$Trt)
times = unique(trt.mesh.dt$Time2)
# 
var_i = expand.grid(trts, times)
#' Function to interpolate irregular mesh into a regular grid
interp_fun = function(x, irregular_mesh_table){
  
  Time_i = var_i[x, 2]
  Trt_i = var_i[x,1]
  
  dt_i = irregular_mesh_table %>% 
    dplyr::filter(Trt == Trt_i & Time2 == Time_i)
  
  dt_h = with(dt_i, akima::interp(x, y, h_mean, duplicate = "mean", nx = 37, ny = 201))$z %>% 
    data.frame() %>% 
    dplyr::mutate(x = row_number() - 1)%>%
    # tibble::rownames_to_column("x") %>% 
    tidyr::pivot_longer(!x, names_to = "y", values_to = "h_mean")
  
  dt_th = with(dt_i, akima::interp(x, y, th_mean, duplicate = "mean", nx = 37, ny = 201))$z %>% 
    data.frame() %>% 
    dplyr::mutate(x = row_number() - 1)%>%
    # tibble::rownames_to_column("x") %>% 
    tidyr::pivot_longer(!x, names_to = "y", values_to = "th_mean")
  
  dt_i = dt_h %>%
    dplyr::left_join(dt_th, by = c("x", "y")) %>% 
    dplyr::mutate(y = readr::parse_number(y) - 1,
                  # x = as.numeric(x) - 1,
                  Trt = Trt_i,
                  Time = Time_i)
  
  return(dt_i)
  
}

#' Because this process requires a substantial resource. Set up parallel processing:
#' 
#' - Setup parallel processing
numcores = parallel::detectCores()
cl <- makeCluster(numcores)
doParallel::registerDoParallel(cl)
# Document parallel processing variables
print(paste("detectCores() = ", numcores)) # number of cores available and requested
#' - Apply function
i.mesh.lst = base::lapply(1:nrow(var_i), interp_fun, irregular_mesh_table = trt.mesh.dt)

i.mesh.dt = bind_rows(i.mesh.lst) %>% 
  dplyr::mutate(Depth = (y - 200)*0.5)
head(i.mesh.dt)
table(i.mesh.dt$Trt, i.mesh.dt$Time)
#' ## Save output table
save_table(i.mesh.dt, pdataDir ,"Interpolated_Mesh_Mean_th_h_Treatment", rds_only = TRUE)
#' - Stop parallel processing
parallel::stopCluster(cl)