## Loop Through Multiple HYDRUS-2D  Project Folders and 
## - Read `A_Level.out` and `Balance.out` files, 
## - Fix Format, and 
## - Export Formatted Tables.
#
## START AT LINE 54 IF .OUT DATA IS ALREADY CONVERTED INTO A SINGLE TABLE
#
#
library(tidyverse)
library(tools)
library(R.utils)
library(janitor)

## Data Directories
wDir <- getwd()
data_path <- file.path(wDir,"Data_Processed")
## Set directories for hydrus and parent directory for all simulations
parent_dir = file.path(wDir, "Data_Raw" ,"Hydrus_Simulations_Out")

## Libraries
source("functions_hydrus2d.R")

## Supply plot numbers and project folder name
n.plots = c(5,6,7,8,
            13,14,15,16,
            21,22,23,24,
            29,30,31,32)
project_folder = "Hydrus2D_notill_irrigation_tomato_20_weeks"

## Read Hydrus-2D output files into tables

## Apply function to outputs
alevel.dt.lst = lapply(n.plots, read.h2d.alevel.out, 
                       parent.dir = parent_dir, project.folder = project_folder)

alevel.dt = bind_rows(alevel.dt.lst)

# Balance Read takes long time! (~ 1 hour)
balance.dt.lst = lapply(n.plots, read.h2d.balance.out, 
                        parent.dir = parent_dir, project.folder = project_folder)

balance.dt = bind_rows(balance.dt.lst)

# Save data as csv
## Export output to correct format.
# alevel_name = paste0(paste("A_LEVEL_", project_folder), c(".csv", ".rds") )
# balance_name = paste0(paste("BALANCE_", project_folder), c(".csv", ".rds") )
# 
# write_csv(alevel.dt,path = file.path(parent_dir, alevel_name[1]))
# write_rds(alevel.dt,path = file.path(parent_dir, alevel_name[2]))
# write_csv(balance.dt,path = file.path(parent_dir, balance_name[1]))
# write_rds(balance.dt,path = file.path(parent_dir, balance_name[2]))

## ************ START HERE IF .OUT FILE IS ALREADY FORMATTED ********************
## Modify Hydurs output files
## Append Plot description using lookup table

## IMPORT FILES
plot.dt = read_csv(file.path("Data_Raw","Plot_Lookup.csv"),
                   col_types = cols(
                     Plot_ID = col_factor(levels = NULL),
                     Winter_Cover = col_factor(levels = NULL),
                     Tillage_Type = col_factor(levels = NULL),
                     Replicate = col_factor(levels = NULL)
                   ) )
layer.dt = read_csv(file.path("Data_Raw", "Layer_Lookup.csv"),
                    col_types = cols(
                      Layer = col_character(),
                      Layer_Number = col_factor(levels = NULL),
                      Layer_Name = col_factor(levels = NULL),
                      Depth_Top = col_number(),
                      Depth_Bottom = col_number(),
                      Depth_Range = col_character()
                    ) )

alevel.dt = read_rds(file = file.path(parent_dir, "A_LEVEL_ Hydrus2D_notill_irrigation_tomato_20_weeks.rds"))
balance.dt = read_rds(file = file.path(parent_dir, "BALANCE_ Hydrus2D_notill_irrigation_tomato_20_weeks.rds"))


a.dt <- alevel.dt %>% 
  dplyr::mutate(Plot_ID = as.factor(Plot_ID)) %>% 
  dplyr::left_join(plot.dt, by = "Plot_ID")



b.dt <- balance.dt %>% 
  dplyr::mutate(Plot_ID = as.factor(Plot_ID)) %>% 
  tidyr::pivot_longer(cols = starts_with("Layer"), 
                      names_to = "Layer",
                      values_to = "Value",
                      values_drop_na = TRUE ) %>% 
  dplyr::arrange(Plot_ID, Layer, Time) %>% 
  left_join(plot.dt, by = "Plot_ID") %>% 
  left_join(layer.dt, by = "Layer")

## remove spinup first 13 months time from Balance data to reduce size
b.dt = b.dt %>% 
  dplyr::filter(Time > 2184)

## Export output to correct format.
write_rds(b.dt, file = file.path(data_path, "Balance.rds"))
write_rds(a.dt, file = file.path(data_path, "A_Level.rds"))
