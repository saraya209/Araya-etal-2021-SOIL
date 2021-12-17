---
title: "Read Hydrus-2D Mesh Data and Save as Formatted Table"
author: "Samuel Araya"
date: 'April 09, 2021'
output:
   html_document:
     toc: true
     keep_md: true
---


```r
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
parent_dir = file.path(wDir,"Hydrus_Simulations_Out")
dataDir <- file.path(wDir,"Data")
pdataDir <- file.path(wDir, "Data_Processed")
outDir <- file.path(wDir,"R_Print")
## Useful Functions
# Function to save tables as RDS and CSV at one time:
save_table <- function(mytable, mypath, myfilename){
  csv_fullpath = file.path(mypath, paste0(myfilename, ".csv") )
  rds_fullpath = file.path(mypath, paste0(myfilename, ".rds") )
  
  write_csv(mytable, path = csv_fullpath)
  write_rds(mytable, path = rds_fullpath)
  
}
# Function to save plots as pdf and png at one time:
save_plot <- function(myplot, myfilename, width, height,
                      mypath = outDir){
  ggsave(plot = myplot, width = width, height = height,
         filename = file.path(mypath, paste0(myfilename,".pdf")))
  
  ggsave(plot = myplot, width = width, height = height,
         filename = file.path(mypath, paste0(myfilename,".png")))
  
}
```

## Read Mesh Data From Hydrus Folder
Supply plot numbers and project folder name


```r
n.plots = c(5,6,7,8,
            13,14,15,16,
            21,22,23,24,
            29,30,31,32)
project_folder = "Hydrus2D_notill_irrigation_tomato_20_weeks"
```

Read in Mesh Dimensions


```r
mesh = readr::read_lines(file = file.path(parent_dir,"MESHTRIA.TXT"),
                         skip = 1, n_max = 1473)

head(mesh)
```

```
## [1] "     1             0             0" "     2            18             0" "     3            18           100"
## [4] "     4             0            80" "     5            18            80" "     6             0            70"
```

```r
tail(mesh)
```

```
## [1] "  1468       6.20009       63.5978" "  1469       10.5758       64.2446" "  1470       9.02565       65.5212"
## [4] "  1471       10.7128       65.1507" "  1472       16.7128       44.6408" "  1473       16.8816       45.8735"
```

Convert columns delimiter from spaces (which are of variable number 
of spaces in between) to commas, i.e., csv file.


```r
mesh = mesh %>% 
  str_squish() %>% # remove consecutive spaces
  str_replace_all(fixed(" "), ",") # replace space with comma
```

Read in mesh csv with new header


```r
mesh.dt = read_csv(mesh, col_names = c("id", "x", "y"))
```

## Read Hydrus `.out` Binary Files

The binary files are read as a single string of numbers.

For each timestep (0 to 3360 hours = 3361 time points)
a grid containing 1473 points is produced.
Before the start of each grid the number is the time
there fore the total number of digs read = 
1473 * 3361 = 4950753

Plus one digit every 1473 for the time =
4950753 + 3361 = 4954114

Function to read in binary out files:


```r
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
```

Read in `.out` files via function

Suction mesh file (`h.out`)


```r
h.dt.lst = lapply(n.plots, read.h2d.mesh.out, 
                  parent.dir = parent_dir, project.folder = project_folder,
                  out_type = "h.out")

h.dt = bind_rows(h.dt.lst)
```

Theta mesh file (`th.out`)


```r
th.dt.lst = lapply(n.plots, read.h2d.mesh.out, 
                   parent.dir = parent_dir, project.folder = project_folder,
                   out_type = "th.out")


th.dt = bind_rows(th.dt.lst)
```

## Save Mesh Files as Table


```r
save_table(th.dt, pdataDir ,"Mesh_th") 
save_table(h.dt, pdataDir ,"Mesh_h") 
```

