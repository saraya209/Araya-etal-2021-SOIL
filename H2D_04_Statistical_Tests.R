#' ---
#' title: "Statistical Tests Hydrus 2D Results"
#' author: "Samuel Araya"
#' date: '`r format(Sys.Date(), "%B %d, %Y")`'
#' output:
#'    html_document:
#'      toc: true
#'      keep_md: true
#' ---
# /*
#
## RENDER CODE: 
rm(list=ls())
library(knitr)
opts_chunk$set(tidy=TRUE, warning=FALSE, cache=FALSE, message=FALSE)
rmarkdown::render(input = "H2D_04_Statistical_Test.R", output_dir = "Reports")
#*/
#'  
#' - Test data normality and equality of variance
#' - Do ANOVA and Tuky's tests and export files 
#' 
#+ set-paths
# Set up file directories/paths relative to the working directory
wDir <- getwd()
dataDir <- file.path(wDir,"Data_Raw")
procDataDir <- file.path(wDir,"Data_Processed")
plotDir <- file.path(wDir,"Plots")
#
#+ import_libraries, warning=FALSE, message=FALSE
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

#' ## Import Tables
#+ Import_Parse, message=F, warning=F
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
#' 
#' ## Tidy up balance data 
#' 
#' - Add time and date columns, 
#' - calculate theta (Volume of water/ volume), and 
#' - Aggregate by Layer_Number and Layer_Name. 
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


#' ## TREATMENT (RE-)NAME AND LEVELS
notill_levels =  c("ST-NO", "ST-CC", "CT-NO", "CT-CC")
notill_labels = c("ST-NO", "ST-CC", "NT-NO", "NT-CC")

#' Convert table to wide and calculate Theta
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


#' ## Statistical tests for dynamic properties 
#' Table grouped by Layer_Number (1-6)
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

#' Table grouped by Layer_Name
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


#' ### Statistical Normality and Homogeneity of Variance Assumption Tests
#' #### Shapiro-Wilk Normality Test

fc.shapiro.tbl <- bfc.dt %>% 
  dplyr::ungroup()%>%
  select(Trt, Layer_Number, hMean, VolumeW, th, wv.diff, th.diff) %>% 
  group_by(Trt, Layer_Number)  %>% 
  summarise_all(shapiro_wrap) %>% 
  dplyr::left_join(layer_number_lookup)

fc.shapiro.tbl %>%
  kbl() %>%
  kable_paper("hover", full_width = F)

save_table(fc.shapiro.tbl, procDataDir, "Dynamic_2HD_FC_Shapiro_Wilk_Test_Layer_Number2")

#' ### Levene’s Homogeneity of Variance Test

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

save_table(fc.levene.tbl, procDataDir, "Dynamic_FC_Levene_Test_Layer_Number2")

#' ## Analysis of Variance and Tukey's Honest Significance Test 
#' ANOVA and Tukey's Test conducted at p = 0.15 (85% confidence).
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

#' ### Tukey's Test Tables with p-level and without Significance Grouping Letters
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



#' ## Repeat all tests for data grouped by Layer Name ----
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

#' 

## Save both tables
save_table(summ_g_tbl2, procDataDir, "LSD_Comparision_Dynamic_2D_FC_grouped_pvalues_015_Layer_Name2")
save_table(summ_p_tbl2, procDataDir, "LSD_Comparision_Dynamic_2D_FC_p_pvalues_015_Layer_Name2")

