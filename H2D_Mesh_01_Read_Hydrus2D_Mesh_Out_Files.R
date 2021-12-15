#' ---
#' title: "Read Hydrus-2D Mesh Data and Save as Formatted Table"
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
rmarkdown::render(input = "H2D_Mesh_01_Read_Hydrus2D_Mesh_Out_Files.R", output_dir = "Reports")
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
library(akima)

## Data Directories
wDir <- getwd()
parent_dir = file.path(wDir, "Data_Raw" ,"Hydrus_Simulations_Out")
dataDir <- file.path(wDir,"Data_Raw")
pdataDir <- file.path(wDir, "Data_Processed")
outDir <- file.path(wDir,"Plots")
## Custom Functions
source("functions_custom.R")
source("functions_hydrus2d.R")
#
#' ## Read Mesh Data From Hydrus Folder
#' Supply plot numbers and project folder name
n.plots = c(5,6,7,8,
            13,14,15,16,
            21,22,23,24,
            29,30,31,32)
project_folder = "Hydrus2D_notill_irrigation_tomato_20_weeks"

#' Read in Mesh Dimensions
mesh = readr::read_lines(file = file.path(parent_dir,"MESHTRIA.TXT"),
                         skip = 1, n_max = 1473)

#head(mesh)
#tail(mesh)

#' Convert columns delimiter from spaces (which are of variable number 
#' of spaces in between) to commas, i.e., csv file.
mesh = mesh %>% 
  str_squish() %>% # remove consecutive spaces
  str_replace_all(fixed(" "), ",") # replace space with comma


#' Read in mesh csv with new header
mesh.dt = read_csv(mesh, col_names = c("id", "x", "y"))


#' ## Read Hydrus `.out` Binary Files
#' 
#' The binary files are read as a single string of numbers.
#' 
#' For each timestep (0 to 3360 hours = 3361 time points)
#' a grid containing 1473 points is produced.
#' Before the start of each grid the number is the time
#' there fore the total number of digits read = 
#' 1473 * 3361 = 4950753
#' 
#' Plus one digit every 1473 for the time =
#' 4950753 + 3361 = 4954114
#' 
#' Function to read in binary out files:
read.h2d.mesh.out <- function(project.id, parent.dir, project.folder, out_type = "th.out"){
  ## Function to read Hydrus-2 .out files and convert them to analysis
  ## friendly tables.
  ## - A_LEVEL.OUT: information printed each time a time dependent boundary is specified (i.e hourly if variable condition table is hourly.)
  
  ### read A_LEVEL.OUT: Pressure heads and cumulative fluxes on the boundary and in the root zone
  project.name = paste(project.folder, project.id, sep = "_")
  project.path = path.expand(file.path(parent.dir, project.name))
  
  h.out = readBin(file.path(project.path, out_type) , numeric(), n = 5959360, size = 4)
  
  time_ind = seq(1, 3361*1474, by = 1474)
  
  #function to split vector by time
  split_to_table = function(ind, x, x_len = 1473){
    t = x[ind]
    start_ind = ind + 1
    end_ind = ind + x_len -1
    d = x[start_ind:end_ind]
    d = tibble(h = d) %>% 
      dplyr::mutate(Time = t,
                    id = row_number())
    return(d)
  }
  
  h.lst = lapply(time_ind, split_to_table, x = h.out)
  h.dt = bind_rows(h.lst)
  
  h.dt = h.dt %>% 
    left_join(mesh.dt, by = "id") %>% 
    dplyr::mutate(Plot_ID = project.id)
  
  
  
  return(h.dt)
  
}


#' Read in `.out` files via function
#' 
#' Suction mesh file (`h.out`)
h.dt.lst = lapply(n.plots, read.h2d.mesh.out, 
                  parent.dir = parent_dir, project.folder = project_folder,
                  out_type = "h.out")

h.dt = bind_rows(h.dt.lst)

#' Theta mesh file (`th.out`)
th.dt.lst = lapply(n.plots, read.h2d.mesh.out, 
                   parent.dir = parent_dir, project.folder = project_folder,
                   out_type = "th.out")


th.dt = bind_rows(th.dt.lst)


#' ## Save Mesh Files as Table
save_table(th.dt, pdataDir ,"Mesh_th") 
save_table(h.dt, pdataDir ,"Mesh_h") 

