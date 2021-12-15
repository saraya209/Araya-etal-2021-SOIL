---
title: "Statistical Tests Hydrus 2D Results"
author: "Samuel Araya"
date: 'December 10, 2021'
output:
   html_document:
     toc: true
     keep_md: true
---
 
- Test data normality and equality of variance
- Do ANOVA and Tuky's tests and export files 



```r
# Set up file directories/paths relative to the working directory
wDir <- getwd()
dataDir <- file.path(wDir,"Data_Raw")
procDataDir <- file.path(wDir,"Data_Processed")
plotDir <- file.path(wDir,"Plots")
#
```

```r
library(tidyverse)
library(viridis) # color for ggplot
library(scales) # to access plot break formatting functions
library(ggExtra)
library(Hmisc) #Extraploation functions
library(agricolae)
library(car) # Levene’s test
library(kableExtra)
library(bayestestR) # area uneder the curve
## Custom Functions
source("functions_custom.R")
## Import Treatment-Plot look up table
dt.look = read.csv(file.path(dataDir, "Treatment_Plot_Lookup.csv"))
```

## Import Tables


```r
## Dynamic simulation data
b.dt <- read_rds(file = file.path(procDataDir, "Balance.rds"))
## look up tables

layer_number_lookup = read_csv(file.path("Data_Raw" , "Layer_Lookup.csv"), 
                               col_types = cols(
                                 Layer_Number = readr::col_factor(),
                                 Depth_Top = col_double(),
                                 Depth_Bottom = col_double(),
                                 Depth_Range = col_character(),
                                 Layer_Name = readr::col_factor(),
                                 Layer_Number_Text = col_character()
                               ) )
layer_name_lookup = read_csv(file.path("Data_Raw" , "Layer_Name_Lookup.csv"),
                             col_types = cols(
                               Layer_Name = readr::col_factor(),
                               Layer_Name_Text = col_character(),
                               Depth_Top = col_double(),
                               Depth_Bottom = col_double(),
                               Depth_Range = col_character()
                             ) )
```


## Tidy up balance data 

- Add time and date columns, 
- calculate theta (Volume of water/ volume), and 
- Aggregate by Layer_Number and Layer_Name. 


```r
time_look <- tibble(Time = (1:3360),
                    Week = rep(seq(1,20), each = 168),
                    Date_time = rep(
                      seq(ymd_h("2018-05-06 1", tz = "America/Los_Angeles"), 
                          ymd_h("2018-05-12 24 PST", tz = "America/Los_Angeles"), 
                          by = 'hours'),
                      times = 20
                    ),
                    Week_day = lubridate::wday(Date_time, label = TRUE),
                    Hour = hour(Date_time) )
```

## TREATMENT (RE-)NAME AND LEVELS


```r
notill_levels =  c("ST-NO", "ST-CC", "CT-NO", "CT-CC")
notill_labels = c("ST-NO", "ST-CC", "NT-NO", "NT-CC")
```

Convert table to wide and calculate Theta


```r
wb.dt = b.dt %>% 
  dplyr::select(-Units) %>% 
  tidyr::pivot_wider(names_from = Variable, values_from = Value ) %>% 
  mutate(th = VolumeW/Volume,
         Trt = factor(paste(Tillage_Type, Winter_Cover, sep = "-"), 
                      levels =  notill_levels,
                      labels = notill_labels, ordered = TRUE)
  ) %>% 
  left_join(time_look, by = "Time")

## Summarize and append date time columns
swb.dt = wb.dt%>%
  # select(Time, Trt, Layer_Number, Layer_Name, Volume, VolumeW, InFlow, hMean, Yield, WatBalT, WatBalR, th)%>%
  group_by(Time, Trt, Layer_Number, Layer_Name)%>%
  summarise_at(vars(Volume:th), list(mean = mean, sd = sd) )
## Save Summary Data.
#save_table(swb.dt, procDataDir ,"Dynamic_2D_Simulation_Balance_Mean_SD") 
```

## Statistical tests for dynamic properties 
Table grouped by Layer_Number (1-6)


```r
bfc.dt = wb.dt %>%
  dplyr::group_by(Trt, Layer_Number, Plot_ID) %>%
  ## 0 at time just before start of final irrigation = week 15
  dplyr::mutate(Time2 = Time - 2352 ) %>%  # Time - (3360-1015) ) %>% 
  ## Filter from irrigation start to 7 days
  dplyr::filter(Time2 >= 0 & Time2 <= 120) %>% # & Soil_layer == "Entire") %>% 
  ## Calculate water content difference
  dplyr::mutate(VolumeW = VolumeW/(pi*18^2),
                wv.diff = VolumeW - first(VolumeW),
                th.diff = th - first(th))  %>% 
  # Time 72 hours after end of irrigation
  dplyr::filter(Time2 == 72) %>% 
  dplyr::left_join(layer_number_lookup)
```

Table grouped by Layer_Name


```r
bfc.dt2 = wb.dt %>%
  dplyr::group_by(Time,Trt, Layer_Name, Plot_ID) %>%
  dplyr::summarise_at(vars(Volume:th), mean, na.rm=TRUE) %>%
  dplyr::left_join(time_look, by = "Time") %>% 
  ## 0 at time of end of final irrigation = week 15
  dplyr::mutate(Time2 = Time - 2352  ) %>% 
  dplyr::group_by(Trt, Layer_Name, Plot_ID) %>%
  ## Filter from irrigation start to 7 days
  dplyr::filter(Time2 >= 0 & Time2 <= 120) %>% # & Soil_layer == "Entire") %>% 
  ## Calculate water content difference
  dplyr::mutate(VolumeW = VolumeW/(pi*18^2),
                wv.diff = VolumeW - first(VolumeW),
                th.diff = th - first(th))  %>% 
  # Time 72 hours after end of irrigation
  dplyr::filter(Time2 == 72) %>% 
  dplyr::left_join(layer_name_lookup)
```

### Statistical Normality and Homogeneity of Variance Assumption Tests
#### Shapiro-Wilk Normality Test


```r
fc.shapiro.tbl <- bfc.dt %>% 
  dplyr::ungroup()%>%
  select(Trt, Layer_Number, hMean, VolumeW, th, wv.diff, th.diff) %>% 
  group_by(Trt, Layer_Number)  %>% 
  summarise_all(shapiro_wrap) %>% 
  dplyr::left_join(layer_number_lookup)

fc.shapiro.tbl %>%
  kbl() %>%
  kable_paper("hover", full_width = F)
```

<table class=" lightable-paper lightable-hover" style='font-family: "Arial Narrow", arial, helvetica, sans-serif; width: auto !important; margin-left: auto; margin-right: auto;'>
 <thead>
  <tr>
   <th style="text-align:left;"> Trt </th>
   <th style="text-align:left;"> Layer_Number </th>
   <th style="text-align:right;"> hMean </th>
   <th style="text-align:right;"> VolumeW </th>
   <th style="text-align:right;"> th </th>
   <th style="text-align:right;"> wv.diff </th>
   <th style="text-align:right;"> th.diff </th>
   <th style="text-align:left;"> Layer </th>
   <th style="text-align:right;"> Depth_Top </th>
   <th style="text-align:right;"> Depth_Bottom </th>
   <th style="text-align:left;"> Depth_Range </th>
   <th style="text-align:left;"> Layer_Name </th>
   <th style="text-align:left;"> Layer_Number_Text </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> ST-NO </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> 0.0541478 </td>
   <td style="text-align:right;"> 0.3103887 </td>
   <td style="text-align:right;"> 0.3103887 </td>
   <td style="text-align:right;"> 0.2402582 </td>
   <td style="text-align:right;"> 0.2402582 </td>
   <td style="text-align:left;"> Layer_0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 100 </td>
   <td style="text-align:left;"> 0 - 100 cm </td>
   <td style="text-align:left;"> Entire </td>
   <td style="text-align:left;"> Entire (0-100 cm) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ST-NO </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 0.2481425 </td>
   <td style="text-align:right;"> 0.6385381 </td>
   <td style="text-align:right;"> 0.6385381 </td>
   <td style="text-align:right;"> 0.5349682 </td>
   <td style="text-align:right;"> 0.5349682 </td>
   <td style="text-align:left;"> Layer_1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:left;"> 0 - 10 cm </td>
   <td style="text-align:left;"> Top </td>
   <td style="text-align:left;"> Top (0-10 cm) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ST-NO </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 0.5106025 </td>
   <td style="text-align:right;"> 0.6250626 </td>
   <td style="text-align:right;"> 0.6250626 </td>
   <td style="text-align:right;"> 0.3354602 </td>
   <td style="text-align:right;"> 0.3354602 </td>
   <td style="text-align:left;"> Layer_2 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:left;"> 10 - 20 cm </td>
   <td style="text-align:left;"> Top </td>
   <td style="text-align:left;"> Top (10-20 cm) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ST-NO </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:right;"> 0.1260961 </td>
   <td style="text-align:right;"> 0.5230467 </td>
   <td style="text-align:right;"> 0.5230467 </td>
   <td style="text-align:right;"> 0.4504983 </td>
   <td style="text-align:right;"> 0.4504983 </td>
   <td style="text-align:left;"> Layer_3 </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:left;"> 20 - 30 cm </td>
   <td style="text-align:left;"> Subsurface </td>
   <td style="text-align:left;"> Subsurface (20-30 cm) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ST-NO </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:right;"> 0.4583059 </td>
   <td style="text-align:right;"> 0.4583990 </td>
   <td style="text-align:right;"> 0.4583990 </td>
   <td style="text-align:right;"> 0.9423063 </td>
   <td style="text-align:right;"> 0.9423063 </td>
   <td style="text-align:left;"> Layer_4 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:left;"> 30 - 40 cm </td>
   <td style="text-align:left;"> Subsurface </td>
   <td style="text-align:left;"> Subsurface (30-40 cm) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ST-NO </td>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:right;"> 0.3391497 </td>
   <td style="text-align:right;"> 0.3129031 </td>
   <td style="text-align:right;"> 0.3129031 </td>
   <td style="text-align:right;"> 0.8264871 </td>
   <td style="text-align:right;"> 0.8264871 </td>
   <td style="text-align:left;"> Layer_5 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:left;"> 40 - 60 cm </td>
   <td style="text-align:left;"> Bottom1 </td>
   <td style="text-align:left;"> Bottom (40-60 cm) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ST-NO </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:right;"> 0.1012382 </td>
   <td style="text-align:right;"> 0.1272303 </td>
   <td style="text-align:right;"> 0.1272303 </td>
   <td style="text-align:right;"> 0.5257745 </td>
   <td style="text-align:right;"> 0.5257745 </td>
   <td style="text-align:left;"> Layer_6 </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:right;"> 100 </td>
   <td style="text-align:left;"> 60 - 100 cm </td>
   <td style="text-align:left;"> Bottom2 </td>
   <td style="text-align:left;"> Bottom (60-100 cm) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ST-CC </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> 0.9337782 </td>
   <td style="text-align:right;"> 0.3173250 </td>
   <td style="text-align:right;"> 0.3173250 </td>
   <td style="text-align:right;"> 0.7076133 </td>
   <td style="text-align:right;"> 0.7076133 </td>
   <td style="text-align:left;"> Layer_0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 100 </td>
   <td style="text-align:left;"> 0 - 100 cm </td>
   <td style="text-align:left;"> Entire </td>
   <td style="text-align:left;"> Entire (0-100 cm) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ST-CC </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 0.7218110 </td>
   <td style="text-align:right;"> 0.5515796 </td>
   <td style="text-align:right;"> 0.5515796 </td>
   <td style="text-align:right;"> 0.7644289 </td>
   <td style="text-align:right;"> 0.7644289 </td>
   <td style="text-align:left;"> Layer_1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:left;"> 0 - 10 cm </td>
   <td style="text-align:left;"> Top </td>
   <td style="text-align:left;"> Top (0-10 cm) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ST-CC </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 0.9620851 </td>
   <td style="text-align:right;"> 0.5385950 </td>
   <td style="text-align:right;"> 0.5385950 </td>
   <td style="text-align:right;"> 0.8207575 </td>
   <td style="text-align:right;"> 0.8207575 </td>
   <td style="text-align:left;"> Layer_2 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:left;"> 10 - 20 cm </td>
   <td style="text-align:left;"> Top </td>
   <td style="text-align:left;"> Top (10-20 cm) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ST-CC </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:right;"> 0.6137947 </td>
   <td style="text-align:right;"> 0.3353352 </td>
   <td style="text-align:right;"> 0.3353352 </td>
   <td style="text-align:right;"> 0.8371237 </td>
   <td style="text-align:right;"> 0.8371237 </td>
   <td style="text-align:left;"> Layer_3 </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:left;"> 20 - 30 cm </td>
   <td style="text-align:left;"> Subsurface </td>
   <td style="text-align:left;"> Subsurface (20-30 cm) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ST-CC </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:right;"> 0.8381354 </td>
   <td style="text-align:right;"> 0.8666773 </td>
   <td style="text-align:right;"> 0.8666773 </td>
   <td style="text-align:right;"> 0.2615490 </td>
   <td style="text-align:right;"> 0.2615490 </td>
   <td style="text-align:left;"> Layer_4 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:left;"> 30 - 40 cm </td>
   <td style="text-align:left;"> Subsurface </td>
   <td style="text-align:left;"> Subsurface (30-40 cm) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ST-CC </td>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:right;"> 0.7329702 </td>
   <td style="text-align:right;"> 0.7205189 </td>
   <td style="text-align:right;"> 0.7205189 </td>
   <td style="text-align:right;"> 0.7783091 </td>
   <td style="text-align:right;"> 0.7783091 </td>
   <td style="text-align:left;"> Layer_5 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:left;"> 40 - 60 cm </td>
   <td style="text-align:left;"> Bottom1 </td>
   <td style="text-align:left;"> Bottom (40-60 cm) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ST-CC </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:right;"> 0.7285081 </td>
   <td style="text-align:right;"> 0.6463636 </td>
   <td style="text-align:right;"> 0.6463636 </td>
   <td style="text-align:right;"> 0.6325687 </td>
   <td style="text-align:right;"> 0.6325687 </td>
   <td style="text-align:left;"> Layer_6 </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:right;"> 100 </td>
   <td style="text-align:left;"> 60 - 100 cm </td>
   <td style="text-align:left;"> Bottom2 </td>
   <td style="text-align:left;"> Bottom (60-100 cm) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NT-NO </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> 0.7804811 </td>
   <td style="text-align:right;"> 0.0965872 </td>
   <td style="text-align:right;"> 0.0965872 </td>
   <td style="text-align:right;"> 0.3244914 </td>
   <td style="text-align:right;"> 0.3244914 </td>
   <td style="text-align:left;"> Layer_0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 100 </td>
   <td style="text-align:left;"> 0 - 100 cm </td>
   <td style="text-align:left;"> Entire </td>
   <td style="text-align:left;"> Entire (0-100 cm) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NT-NO </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 0.8621371 </td>
   <td style="text-align:right;"> 0.0777958 </td>
   <td style="text-align:right;"> 0.0777958 </td>
   <td style="text-align:right;"> 0.4848178 </td>
   <td style="text-align:right;"> 0.4848178 </td>
   <td style="text-align:left;"> Layer_1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:left;"> 0 - 10 cm </td>
   <td style="text-align:left;"> Top </td>
   <td style="text-align:left;"> Top (0-10 cm) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NT-NO </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 0.6763046 </td>
   <td style="text-align:right;"> 0.1133844 </td>
   <td style="text-align:right;"> 0.1133844 </td>
   <td style="text-align:right;"> 0.4925834 </td>
   <td style="text-align:right;"> 0.4925834 </td>
   <td style="text-align:left;"> Layer_2 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:left;"> 10 - 20 cm </td>
   <td style="text-align:left;"> Top </td>
   <td style="text-align:left;"> Top (10-20 cm) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NT-NO </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:right;"> 0.6436477 </td>
   <td style="text-align:right;"> 0.3728631 </td>
   <td style="text-align:right;"> 0.3728631 </td>
   <td style="text-align:right;"> 0.1094605 </td>
   <td style="text-align:right;"> 0.1094605 </td>
   <td style="text-align:left;"> Layer_3 </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:left;"> 20 - 30 cm </td>
   <td style="text-align:left;"> Subsurface </td>
   <td style="text-align:left;"> Subsurface (20-30 cm) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NT-NO </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:right;"> 0.5412925 </td>
   <td style="text-align:right;"> 0.4794724 </td>
   <td style="text-align:right;"> 0.4794724 </td>
   <td style="text-align:right;"> 0.1009539 </td>
   <td style="text-align:right;"> 0.1009539 </td>
   <td style="text-align:left;"> Layer_4 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:left;"> 30 - 40 cm </td>
   <td style="text-align:left;"> Subsurface </td>
   <td style="text-align:left;"> Subsurface (30-40 cm) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NT-NO </td>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:right;"> 0.3416712 </td>
   <td style="text-align:right;"> 0.3379842 </td>
   <td style="text-align:right;"> 0.3379842 </td>
   <td style="text-align:right;"> 0.4393434 </td>
   <td style="text-align:right;"> 0.4393434 </td>
   <td style="text-align:left;"> Layer_5 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:left;"> 40 - 60 cm </td>
   <td style="text-align:left;"> Bottom1 </td>
   <td style="text-align:left;"> Bottom (40-60 cm) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NT-NO </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:right;"> 0.2974927 </td>
   <td style="text-align:right;"> 0.2246237 </td>
   <td style="text-align:right;"> 0.2246237 </td>
   <td style="text-align:right;"> 0.6544757 </td>
   <td style="text-align:right;"> 0.6544757 </td>
   <td style="text-align:left;"> Layer_6 </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:right;"> 100 </td>
   <td style="text-align:left;"> 60 - 100 cm </td>
   <td style="text-align:left;"> Bottom2 </td>
   <td style="text-align:left;"> Bottom (60-100 cm) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NT-CC </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> 0.4644436 </td>
   <td style="text-align:right;"> 0.5933979 </td>
   <td style="text-align:right;"> 0.5933979 </td>
   <td style="text-align:right;"> 0.8609021 </td>
   <td style="text-align:right;"> 0.8609021 </td>
   <td style="text-align:left;"> Layer_0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 100 </td>
   <td style="text-align:left;"> 0 - 100 cm </td>
   <td style="text-align:left;"> Entire </td>
   <td style="text-align:left;"> Entire (0-100 cm) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NT-CC </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 0.7569861 </td>
   <td style="text-align:right;"> 0.7507493 </td>
   <td style="text-align:right;"> 0.7507493 </td>
   <td style="text-align:right;"> 0.6220223 </td>
   <td style="text-align:right;"> 0.6220223 </td>
   <td style="text-align:left;"> Layer_1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:left;"> 0 - 10 cm </td>
   <td style="text-align:left;"> Top </td>
   <td style="text-align:left;"> Top (0-10 cm) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NT-CC </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 0.9065926 </td>
   <td style="text-align:right;"> 0.9767714 </td>
   <td style="text-align:right;"> 0.9767714 </td>
   <td style="text-align:right;"> 0.5946898 </td>
   <td style="text-align:right;"> 0.5946898 </td>
   <td style="text-align:left;"> Layer_2 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:left;"> 10 - 20 cm </td>
   <td style="text-align:left;"> Top </td>
   <td style="text-align:left;"> Top (10-20 cm) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NT-CC </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:right;"> 0.3817368 </td>
   <td style="text-align:right;"> 0.3283049 </td>
   <td style="text-align:right;"> 0.3283049 </td>
   <td style="text-align:right;"> 0.9814703 </td>
   <td style="text-align:right;"> 0.9814703 </td>
   <td style="text-align:left;"> Layer_3 </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:left;"> 20 - 30 cm </td>
   <td style="text-align:left;"> Subsurface </td>
   <td style="text-align:left;"> Subsurface (20-30 cm) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NT-CC </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:right;"> 0.6244271 </td>
   <td style="text-align:right;"> 0.6299076 </td>
   <td style="text-align:right;"> 0.6299076 </td>
   <td style="text-align:right;"> 0.4156573 </td>
   <td style="text-align:right;"> 0.4156573 </td>
   <td style="text-align:left;"> Layer_4 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:left;"> 30 - 40 cm </td>
   <td style="text-align:left;"> Subsurface </td>
   <td style="text-align:left;"> Subsurface (30-40 cm) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NT-CC </td>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:right;"> 0.3412501 </td>
   <td style="text-align:right;"> 0.3265155 </td>
   <td style="text-align:right;"> 0.3265155 </td>
   <td style="text-align:right;"> 0.1931766 </td>
   <td style="text-align:right;"> 0.1931766 </td>
   <td style="text-align:left;"> Layer_5 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:left;"> 40 - 60 cm </td>
   <td style="text-align:left;"> Bottom1 </td>
   <td style="text-align:left;"> Bottom (40-60 cm) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NT-CC </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:right;"> 0.3232567 </td>
   <td style="text-align:right;"> 0.2588479 </td>
   <td style="text-align:right;"> 0.2588479 </td>
   <td style="text-align:right;"> 0.2904531 </td>
   <td style="text-align:right;"> 0.2904531 </td>
   <td style="text-align:left;"> Layer_6 </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:right;"> 100 </td>
   <td style="text-align:left;"> 60 - 100 cm </td>
   <td style="text-align:left;"> Bottom2 </td>
   <td style="text-align:left;"> Bottom (60-100 cm) </td>
  </tr>
</tbody>
</table>

```r
save_table(fc.shapiro.tbl, procDataDir, "Dynamic_2HD_FC_Shapiro_Wilk_Test_Layer_Number2")
```

### Levene’s Homogeneity of Variance Test


```r
fc.levene.tbl <- bfc.dt %>% 
  dplyr::ungroup()%>%
  select(Trt, Layer_Number, hMean, VolumeW, th, wv.diff, th.diff) %>% 
  gather(Variable, value = "Value", -Trt, -Layer_Number)%>%
  group_by(Variable, Layer_Number) %>% 
  do(levene_p = levene_wrap(.$Value, .$Trt)) %>% 
  unnest(cols = c(levene_p))%>% 
  dplyr::left_join(layer_number_lookup)

fc.levene.tbl %>%
  kbl() %>%
  kable_paper("hover", full_width = F)
```

<table class=" lightable-paper lightable-hover" style='font-family: "Arial Narrow", arial, helvetica, sans-serif; width: auto !important; margin-left: auto; margin-right: auto;'>
 <thead>
  <tr>
   <th style="text-align:left;"> Variable </th>
   <th style="text-align:left;"> Layer_Number </th>
   <th style="text-align:right;"> levene_p </th>
   <th style="text-align:left;"> Layer </th>
   <th style="text-align:right;"> Depth_Top </th>
   <th style="text-align:right;"> Depth_Bottom </th>
   <th style="text-align:left;"> Depth_Range </th>
   <th style="text-align:left;"> Layer_Name </th>
   <th style="text-align:left;"> Layer_Number_Text </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> hMean </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> 0.3560456 </td>
   <td style="text-align:left;"> Layer_0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 100 </td>
   <td style="text-align:left;"> 0 - 100 cm </td>
   <td style="text-align:left;"> Entire </td>
   <td style="text-align:left;"> Entire (0-100 cm) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> hMean </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 0.3719395 </td>
   <td style="text-align:left;"> Layer_1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:left;"> 0 - 10 cm </td>
   <td style="text-align:left;"> Top </td>
   <td style="text-align:left;"> Top (0-10 cm) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> hMean </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 0.2216506 </td>
   <td style="text-align:left;"> Layer_2 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:left;"> 10 - 20 cm </td>
   <td style="text-align:left;"> Top </td>
   <td style="text-align:left;"> Top (10-20 cm) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> hMean </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:right;"> 0.3842187 </td>
   <td style="text-align:left;"> Layer_3 </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:left;"> 20 - 30 cm </td>
   <td style="text-align:left;"> Subsurface </td>
   <td style="text-align:left;"> Subsurface (20-30 cm) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> hMean </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:right;"> 0.2686709 </td>
   <td style="text-align:left;"> Layer_4 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:left;"> 30 - 40 cm </td>
   <td style="text-align:left;"> Subsurface </td>
   <td style="text-align:left;"> Subsurface (30-40 cm) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> hMean </td>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:right;"> 0.2598430 </td>
   <td style="text-align:left;"> Layer_5 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:left;"> 40 - 60 cm </td>
   <td style="text-align:left;"> Bottom1 </td>
   <td style="text-align:left;"> Bottom (40-60 cm) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> hMean </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:right;"> 0.6381664 </td>
   <td style="text-align:left;"> Layer_6 </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:right;"> 100 </td>
   <td style="text-align:left;"> 60 - 100 cm </td>
   <td style="text-align:left;"> Bottom2 </td>
   <td style="text-align:left;"> Bottom (60-100 cm) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> th </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> 0.8498855 </td>
   <td style="text-align:left;"> Layer_0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 100 </td>
   <td style="text-align:left;"> 0 - 100 cm </td>
   <td style="text-align:left;"> Entire </td>
   <td style="text-align:left;"> Entire (0-100 cm) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> th </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 0.8493800 </td>
   <td style="text-align:left;"> Layer_1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:left;"> 0 - 10 cm </td>
   <td style="text-align:left;"> Top </td>
   <td style="text-align:left;"> Top (0-10 cm) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> th </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 0.7390191 </td>
   <td style="text-align:left;"> Layer_2 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:left;"> 10 - 20 cm </td>
   <td style="text-align:left;"> Top </td>
   <td style="text-align:left;"> Top (10-20 cm) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> th </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:right;"> 0.2050413 </td>
   <td style="text-align:left;"> Layer_3 </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:left;"> 20 - 30 cm </td>
   <td style="text-align:left;"> Subsurface </td>
   <td style="text-align:left;"> Subsurface (20-30 cm) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> th </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:right;"> 0.2553104 </td>
   <td style="text-align:left;"> Layer_4 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:left;"> 30 - 40 cm </td>
   <td style="text-align:left;"> Subsurface </td>
   <td style="text-align:left;"> Subsurface (30-40 cm) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> th </td>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:right;"> 0.3220175 </td>
   <td style="text-align:left;"> Layer_5 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:left;"> 40 - 60 cm </td>
   <td style="text-align:left;"> Bottom1 </td>
   <td style="text-align:left;"> Bottom (40-60 cm) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> th </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:right;"> 0.6280274 </td>
   <td style="text-align:left;"> Layer_6 </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:right;"> 100 </td>
   <td style="text-align:left;"> 60 - 100 cm </td>
   <td style="text-align:left;"> Bottom2 </td>
   <td style="text-align:left;"> Bottom (60-100 cm) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> th.diff </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> 0.4295345 </td>
   <td style="text-align:left;"> Layer_0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 100 </td>
   <td style="text-align:left;"> 0 - 100 cm </td>
   <td style="text-align:left;"> Entire </td>
   <td style="text-align:left;"> Entire (0-100 cm) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> th.diff </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 0.9662661 </td>
   <td style="text-align:left;"> Layer_1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:left;"> 0 - 10 cm </td>
   <td style="text-align:left;"> Top </td>
   <td style="text-align:left;"> Top (0-10 cm) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> th.diff </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 0.6259071 </td>
   <td style="text-align:left;"> Layer_2 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:left;"> 10 - 20 cm </td>
   <td style="text-align:left;"> Top </td>
   <td style="text-align:left;"> Top (10-20 cm) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> th.diff </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:right;"> 0.0144621 </td>
   <td style="text-align:left;"> Layer_3 </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:left;"> 20 - 30 cm </td>
   <td style="text-align:left;"> Subsurface </td>
   <td style="text-align:left;"> Subsurface (20-30 cm) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> th.diff </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:right;"> 0.0115252 </td>
   <td style="text-align:left;"> Layer_4 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:left;"> 30 - 40 cm </td>
   <td style="text-align:left;"> Subsurface </td>
   <td style="text-align:left;"> Subsurface (30-40 cm) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> th.diff </td>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:right;"> 0.0458625 </td>
   <td style="text-align:left;"> Layer_5 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:left;"> 40 - 60 cm </td>
   <td style="text-align:left;"> Bottom1 </td>
   <td style="text-align:left;"> Bottom (40-60 cm) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> th.diff </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:right;"> 0.2640100 </td>
   <td style="text-align:left;"> Layer_6 </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:right;"> 100 </td>
   <td style="text-align:left;"> 60 - 100 cm </td>
   <td style="text-align:left;"> Bottom2 </td>
   <td style="text-align:left;"> Bottom (60-100 cm) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> VolumeW </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> 0.8498855 </td>
   <td style="text-align:left;"> Layer_0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 100 </td>
   <td style="text-align:left;"> 0 - 100 cm </td>
   <td style="text-align:left;"> Entire </td>
   <td style="text-align:left;"> Entire (0-100 cm) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> VolumeW </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 0.8493800 </td>
   <td style="text-align:left;"> Layer_1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:left;"> 0 - 10 cm </td>
   <td style="text-align:left;"> Top </td>
   <td style="text-align:left;"> Top (0-10 cm) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> VolumeW </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 0.7390191 </td>
   <td style="text-align:left;"> Layer_2 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:left;"> 10 - 20 cm </td>
   <td style="text-align:left;"> Top </td>
   <td style="text-align:left;"> Top (10-20 cm) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> VolumeW </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:right;"> 0.2050413 </td>
   <td style="text-align:left;"> Layer_3 </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:left;"> 20 - 30 cm </td>
   <td style="text-align:left;"> Subsurface </td>
   <td style="text-align:left;"> Subsurface (20-30 cm) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> VolumeW </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:right;"> 0.2553104 </td>
   <td style="text-align:left;"> Layer_4 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:left;"> 30 - 40 cm </td>
   <td style="text-align:left;"> Subsurface </td>
   <td style="text-align:left;"> Subsurface (30-40 cm) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> VolumeW </td>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:right;"> 0.3220175 </td>
   <td style="text-align:left;"> Layer_5 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:left;"> 40 - 60 cm </td>
   <td style="text-align:left;"> Bottom1 </td>
   <td style="text-align:left;"> Bottom (40-60 cm) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> VolumeW </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:right;"> 0.6280274 </td>
   <td style="text-align:left;"> Layer_6 </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:right;"> 100 </td>
   <td style="text-align:left;"> 60 - 100 cm </td>
   <td style="text-align:left;"> Bottom2 </td>
   <td style="text-align:left;"> Bottom (60-100 cm) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> wv.diff </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> 0.4295345 </td>
   <td style="text-align:left;"> Layer_0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 100 </td>
   <td style="text-align:left;"> 0 - 100 cm </td>
   <td style="text-align:left;"> Entire </td>
   <td style="text-align:left;"> Entire (0-100 cm) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> wv.diff </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 0.9662661 </td>
   <td style="text-align:left;"> Layer_1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:left;"> 0 - 10 cm </td>
   <td style="text-align:left;"> Top </td>
   <td style="text-align:left;"> Top (0-10 cm) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> wv.diff </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 0.6259071 </td>
   <td style="text-align:left;"> Layer_2 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:left;"> 10 - 20 cm </td>
   <td style="text-align:left;"> Top </td>
   <td style="text-align:left;"> Top (10-20 cm) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> wv.diff </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:right;"> 0.0144621 </td>
   <td style="text-align:left;"> Layer_3 </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:left;"> 20 - 30 cm </td>
   <td style="text-align:left;"> Subsurface </td>
   <td style="text-align:left;"> Subsurface (20-30 cm) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> wv.diff </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:right;"> 0.0115252 </td>
   <td style="text-align:left;"> Layer_4 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:left;"> 30 - 40 cm </td>
   <td style="text-align:left;"> Subsurface </td>
   <td style="text-align:left;"> Subsurface (30-40 cm) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> wv.diff </td>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:right;"> 0.0458625 </td>
   <td style="text-align:left;"> Layer_5 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:left;"> 40 - 60 cm </td>
   <td style="text-align:left;"> Bottom1 </td>
   <td style="text-align:left;"> Bottom (40-60 cm) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> wv.diff </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:right;"> 0.2640100 </td>
   <td style="text-align:left;"> Layer_6 </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:right;"> 100 </td>
   <td style="text-align:left;"> 60 - 100 cm </td>
   <td style="text-align:left;"> Bottom2 </td>
   <td style="text-align:left;"> Bottom (60-100 cm) </td>
  </tr>
</tbody>
</table>

```r
save_table(fc.levene.tbl, procDataDir, "Dynamic_FC_Levene_Test_Layer_Number2")
```

## Analysis of Variance and Tukey's Honest Significance Test 
ANOVA and Tukey's Test conducted at p = 0.15 (85% confidence).


```r
# Create list of ANOVA tables for each treatment and depth
summ_g_lst_tbl = bfc.dt%>%
  dplyr::ungroup()%>%
  #mutate(Depth_ID = factor(Depth_ID, levels = c("T", "B")))%>%
  select(Trt, Layer_Number, hMean, VolumeW, th, wv.diff, th.diff) %>% 
  gather(Variable, value = "Value", -Trt, -Layer_Number)%>%
  group_by(Layer_Number, Variable)%>%
  do(compare = compare_fun(.$Value, .$Trt)) %>% 
  # Create one unique column for list naming later.
  dplyr::mutate(depth_var = paste(Layer_Number, Variable, sep = "-"))

#Merge Anova Tables into one
# Convert to named list
summ_g_lst = setNames(summ_g_lst_tbl$compare, 
                      summ_g_lst_tbl$depth_var)
# Bind the named list
summ_g_tbl = bind_rows(summ_g_lst, .id = "depth_var")%>% 
  tidyr::separate(depth_var, c("Layer_Number", "Variable"),"-") %>% 
  dplyr::left_join(layer_number_lookup)
```

### Tukey's Test Tables with p-level and without Significance Grouping Letters


```r
# Create list of ANOVA tables with out significance grouping letters for each treatment and depth
summ_p_lst_tbl = bfc.dt%>%
  dplyr::ungroup()%>%
  #mutate(Depth_ID = factor(Depth_ID, levels = c("T", "B")))%>%
  select(Trt, Layer_Number, hMean, VolumeW, th, wv.diff, th.diff) %>% 
  gather(Variable, value = "Value", -Trt, -Layer_Number)%>%
  group_by(Layer_Number, Variable)%>%
  do(compare = compare_fun(.$Value, .$Trt, letter_group = FALSE)) %>% 
  # Create one unique column for list naming later.
  dplyr::mutate(depth_var = paste(Layer_Number, Variable, sep = "-"))

#Merge Anova Tables into one
# Convert to named list
summ_p_lst = setNames(summ_p_lst_tbl$compare, 
                      summ_p_lst_tbl$depth_var)
# Bind the named list
summ_p_tbl = bind_rows(summ_p_lst, .id = "depth_var")%>% 
  tidyr::separate(depth_var, c("Layer_Number", "Variable"),"-") %>% 
  dplyr::left_join(layer_number_lookup)


## Save both tables
save_table(summ_g_tbl, procDataDir, "LSD_Comparision_Dynamic_2D_FC_grouped_pvalues_015_Layer_Number2")
save_table(summ_p_tbl, procDataDir, "LSD_Comparision_Dynamic_2D_FC_p_pvalues_015_Layer_Number2")
```

## Repeat all tests for data grouped by Layer Name ----


```r
# Create list of ANOVA tables for each treatment and depth
summ_g_lst_tbl2 = bfc.dt2%>%
  dplyr::ungroup()%>%
  #mutate(Depth_ID = factor(Depth_ID, levels = c("T", "B")))%>%
  select(Trt, Layer_Name, hMean, VolumeW, th, wv.diff, th.diff) %>% 
  gather(Variable, value = "Value", -Trt, -Layer_Name)%>%
  group_by(Layer_Name, Variable)%>%
  do(compare = compare_fun(.$Value, .$Trt)) %>% 
  # Create one unique column for list naming later.
  dplyr::mutate(depth_var = paste(Layer_Name, Variable, sep = "-"))

#Merge Anova Tables into one
# Convert to named list
summ_g_lst2 = setNames(summ_g_lst_tbl2$compare, 
                       summ_g_lst_tbl2$depth_var)
# Bind the named list
summ_g_tbl2 = bind_rows(summ_g_lst2, .id = "depth_var")%>% 
  tidyr::separate(depth_var, c("Layer_Name", "Variable"),"-") %>% 
  dplyr::left_join(layer_name_lookup)


# Create list of ANOVA tables with out significance grouping letters for each treatment and depth
summ_p_lst_tbl2 = bfc.dt2%>%
  dplyr::ungroup()%>%
  #mutate(Depth_ID = factor(Depth_ID, levels = c("T", "B")))%>%
  select(Trt, Layer_Name, hMean, VolumeW, th, wv.diff, th.diff) %>% 
  gather(Variable, value = "Value", -Trt, -Layer_Name)%>%
  group_by(Layer_Name, Variable)%>%
  do(compare = compare_fun(.$Value, .$Trt, letter_group = FALSE)) %>% 
  # Create one unique column for list naming later.
  dplyr::mutate(depth_var = paste(Layer_Name, Variable, sep = "-"))

#Merge Anova Tables into one
# Convert to named list
summ_p_lst2 = setNames(summ_p_lst_tbl2$compare, 
                       summ_p_lst_tbl2$depth_var)
# Bind the named list
summ_p_tbl2 = bind_rows(summ_p_lst2, .id = "depth_var")%>% 
  tidyr::separate(depth_var, c("Layer_Name", "Variable"),"-") %>% 
  dplyr::left_join(layer_name_lookup)
```




```r
## Save both tables
save_table(summ_g_tbl2, procDataDir, "LSD_Comparision_Dynamic_2D_FC_grouped_pvalues_015_Layer_Name2")
save_table(summ_p_tbl2, procDataDir, "LSD_Comparision_Dynamic_2D_FC_p_pvalues_015_Layer_Name2")
```

