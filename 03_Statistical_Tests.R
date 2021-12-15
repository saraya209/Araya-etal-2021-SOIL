#' ---
#' title: "Statistical Tests and Comparisons"
#' author: "Samuel Araya"
#' date: '`r format(Sys.Date(), "%B %d, %Y")`'
#' output:
#'    html_document:
#'      toc: true
#'      keep_md: true
#' ---
# /*
# This Section is NOT Executed. Use only to RENDER REPORTS:
rm(list = ls())
library(knitr)
opts_chunk$set(
  tidy = TRUE,
  warning = FALSE,
  cache = FALSE,
  message = FALSE
)
rmarkdown::render(input = "03_Statistical_Tests.R", output_dir = "Reports")
#
# */

#' This script uses data exported from `02_Make_Lab_Data_Analysis_Ready.R` script and:
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

#
#' ## Import Tables
#+ import-data, message=FALSE
## Read summary 
dt <- read_rds(file = file.path(procDataDir, "HYPROP_Fit_bi_Summary.rds")) 
dt.seq <- read_rds(file = file.path(procDataDir, "Interpolated_Extraploated_spline_K_theta_pF.rds"))

#'
#' ## Clean Up and Fix Table
#' Calculate Field Capacity at 300 and 100 cm (`FC` and `FC100`), and
#' calculate wilting point at 1,500 cm (`WP`).
dt.summ <- dt.seq%>%
  group_by(Sample_ID, Trt)%>%
  summarise(FC = (approx(h,theta,xout = log10(33))[[2]])*0.01,
            FC100 = (approx(h,theta,xout = log10(10))[[2]])*0.01,
            WP = (approx(h,theta,xout = log10(1500))[[2]])*0.01,
            K100 = (approx(h, K, xout = log10(10))[[2]])*0.01)%>%
  mutate(PAW = FC - WP,
         PAW100 = FC100 - WP) %>% 
  left_join(dt.look, by = "Sample_ID")
#' 
#' Combine FC and WP data with point measurement data for statistical analysis
#' 
### TREATMENT (RE-)NAME AND LEVELS
notill_levels =  c("ST-NO", "ST-CC", "CT-NO", "CT-CC")
notill_labels = c("ST-NO", "ST-CC", "NT-NO", "NT-CC")

dt.pts = dt%>%
  select(Sample_ID, BD, Porosity, Ks, th_r, th_s) %>% 
  mutate(Ks = log10(Ks)) %>% 
  right_join(dt.summ, by = "Sample_ID") %>% 
  dplyr::mutate(
    Trt = paste(Tillage_Type, Winter_Cover, sep = "-"),
    Trt = factor(
      Trt,
      levels =  notill_levels,
      labels = notill_labels,
      ordered = TRUE
    ) )

#' ## Statistical Normality and Homogeneity of Variance Assumption Tests
#' ### Shapiro-Wilk Normality Test
#' 
#' With 95 % (99% for few) confidence, all the data showed no 
#' significant departure from normality.

shapiro.tbl <- dt.pts %>% 
  select(Trt, Depth_text, BD, Porosity, Ks, K100, FC, FC100, WP, PAW, PAW100) %>% 
  group_by(Trt, Depth_text)  %>% 
  summarise_all(shapiro_wrap)

shapiro.tbl %>%
  kbl() %>%
  kable_paper("hover", full_width = F)

save_table(shapiro.tbl, procDataDir, "Shapiro_Wilk_Test_measured_SHP")

#' ### Levene’s Homogeneity of Variance Test
#' 
#' #' With 95 % (99% for few) confidence level, all the data showed no 
#' significant evidence that the variance are not equal.
#' 

levene.tbl <- dt.pts %>% 
  select(Trt, Depth_text, BD, Porosity, Ks, K100, FC, FC100, WP, PAW, PAW100) %>%
  gather(Variable, value = "Value", -Trt, -Depth_text)%>%
  group_by(Variable, Depth_text) %>% 
  do(levene_p = levene_wrap(.$Value, .$Trt)) %>% 
  unnest(cols = c(levene_p))

levene.tbl %>%
  kbl() %>%
  kable_paper("hover", full_width = F)

save_table(levene.tbl, procDataDir, "Levene_Test_measured_SHP")

#' ## Analysis of Variance and Tukey's Honest Significance Test 
#' ANOVA and Tukey's Test conducted at p = 0.15 (85% confidence).
#' 
#' ### Tukey's Test Tables with Significance Grouping Letters
summ_grouped_lst_tbl = dt.pts%>%
  mutate(Depth_ID = factor(Depth_ID, levels = c("T", "B")))%>%
  select(Trt, Depth_ID,BD, Porosity, Ks, K100, FC, FC100, WP, PAW, PAW100) %>%
  gather(Variable, value = "Value", -Trt, -Depth_ID)%>%
  group_by(Depth_ID, Variable)%>%
  do(compare = compare_fun(.$Value, .$Trt, p = 0.15)) %>% 
  # Create one unique column for list naming later.
  dplyr::mutate(depth_var = paste(Depth_ID, Variable, sep = "-"))

# Combine List of tables into one table
# Convert to named list
summ_grouped_lst = setNames(summ_grouped_lst_tbl$compare, 
                            summ_grouped_lst_tbl$depth_var)
# Bind the named list
summ_grouped_tbl = bind_rows(summ_grouped_lst, .id = "depth_var")%>% 
  tidyr::separate(depth_var, c("Depth_ID", "Variable"),"-")

#' ### Tukey's Test Tables with p-level and without Significance Grouping Letters
summ_p_lst_tbl = dt.pts%>%
  mutate(Depth_ID = factor(Depth_ID, levels = c("T", "B")))%>%
  select(Trt, Depth_ID,BD, Porosity, Ks, K100, FC, FC100, WP, PAW, PAW100) %>%
  gather(Variable, value = "Value", -Trt, -Depth_ID)%>%
  group_by(Depth_ID, Variable)%>%
  do(compare = compare_fun(.$Value, .$Trt, p = 0.15, letter_group = FALSE)) %>% 
  # Create one unique column for list naming later.
  dplyr::mutate(depth_var = paste(Depth_ID, Variable, sep = "-"))

# Combine List of tables into one table
# Convert to named list
summ_p_lst = setNames(summ_p_lst_tbl$compare, 
                      summ_p_lst_tbl$depth_var)
# Bind the named list
summ_p_tbl = bind_rows(summ_p_lst, .id = "depth_var")%>% 
  tidyr::separate(depth_var, c("Depth_ID", "Variable"),"-")

#' ### Export Both Tukey's Table 
save_table(summ_grouped_tbl, procDataDir, "LSD_Comparision_Grouped_p_015")
save_table(summ_p_tbl, procDataDir, "LSD_Comparision_pvalues_p_015")

#' ## Calculate Pore Size Range Means and Statistical Test
#' 
#' Divide pore sizes into characteristic diameters.
#' diameter in micro meters:
#' 
#' 1. `<0.2`: intra-microaggregates
#' 2. `0.2-10`: intra-aggregates
#' 3. `10-50`: small macro pores and biopores (e.g., fine roots)
#' 4. `50-1000`: large macropores, cracks, and biopores (e.g., coarse roots)
#' 
## Groups
psd_levels = c("<0.2", "0.2-10", "10-50", "50-1000")

dt.g = dt.seq %>%
  dplyr::select(Sample_ID,
                Trt,
                Depth_text,
                h,
                h.cm,
                Porosity,
                r,
                ln.r,
                Sr.spl,
                Sr.diff) %>%
  dplyr::filter(r <= 1000 * 2) %>%
  dplyr::mutate(
    psd_group = case_when(
      r <= 0.2 * 2 ~ "<0.2" ,
      r > 0.2 * 2 &
        r  <= 10 * 2 ~ "0.2-10",
      r > 10 * 2 &
        r  <= 50 * 2 ~ "10-50",
      r > 50 * 2 &
        r  <= 1000 * 2 ~ "50-1000"
    ),
    psd_group = factor(psd_group, levels = psd_levels)
  ) %>%
  dplyr::group_by(Sample_ID, Trt, Depth_text, psd_group) %>%
  dplyr::mutate(
    ln.r_area = area_under_curve(x = ln.r,
                                 y = Sr.diff,
                                 method = "trapezoid"),
    ln.r_area_porosity = ln.r_area / Porosity
  ) %>%
  dplyr::group_by(Trt, Depth_text, psd_group) 

#' ### PSD Statistical Analysis
#' Create a long table for statistical analysis
dt.gl = dt.g%>%
  dplyr::select(Trt, Depth_text, psd_group, Sr.diff, ln.r_area, ln.r_area_porosity) %>% 
  tidyr::pivot_longer(!c(Trt, Depth_text, psd_group),
                      names_to = "Variable", 
                      values_to = "Value") %>%
  group_by(Depth_text, psd_group, Variable)

#' Test normality
shapiro.tbl <- dt.g %>% 
  dplyr::select(Trt, Depth_text, psd_group, Sr.diff, ln.r_area, ln.r_area_porosity) %>% 
  dplyr::group_by(Trt, Depth_text, psd_group) %>%  
  dplyr::summarise_all(shapiro_wrap)

write_csv(shapiro.tbl, file = file.path(procDataDir, "PSD_Shapiro_Wilk_Test.csv"))

# Levene’s test
levene.tbl <- dt.gl %>%  
  do(levene_p = levene_wrap(.$Value, .$Trt)) %>% 
  unnest(cols = c(levene_p))

write_csv(levene.tbl, file = file.path(procDataDir, "PSD_Levene_Test.csv"))
## Anova and Tuky's test table
compare_fun = function(y, trt_var){
  dt.tbl = tibble(value = y, Trt = trt_var)
  model = aov(y~Trt, data = dt.tbl)
  out = HSD.test(model, trt = "Trt", alpha = 0.15, group = TRUE)
  means.tbl = rownames_to_column(out$means)
  group.tbl = rownames_to_column(out$groups)[,-2]
  summ.tbl = left_join(means.tbl, group.tbl, by = "rowname")
  summ.tbl = summ.tbl%>%
    rename(Trt = rowname, mean = y) %>% 
    mutate(stder = std/sqrt(r))
  return(summ.tbl)
}
# Create list of ANOVA tables for each treatment and depth
summ.lst = dt.gl %>%
  do(compare = compare_fun(.$Value, .$Trt))
#Merge Anova Tables into one

summ.tbl = tibble()
for (i in 1:nrow(summ.lst)){
  i.tbl = summ.lst$compare[[i]] %>%
    mutate(Depth_text = summ.lst$Depth_text[i],
           psd_group = summ.lst$psd_group[i],
           Variable = summ.lst$Variable[i])
  summ.tbl = bind_rows(summ.tbl, i.tbl)
}

# Same but with p value not grouped
compare2_fun = function(y, trt_var){
  dt.tbl = tibble(value = y, Trt = trt_var)
  model = aov(y~Trt, data = dt.tbl)
  out = HSD.test(model, trt = "Trt", alpha = 0.15, group = F)
  p.tbl = rownames_to_column(out$comparison)
  summ.tbl = p.tbl%>%
    rename(Comparision = rowname)
  return(summ.tbl)
}

summ.lst2 = dt.gl %>% 
  do(compare = compare2_fun(.$Value, .$Trt))

#Merge Anova Tables into one

summ2.tbl = tibble()
for (i in 1:nrow(summ.lst2)){
  i.tbl = summ.lst2$compare[[i]] %>%
    mutate(Depth_text = summ.lst$Depth_text[i],
           psd_group = summ.lst$psd_group[i],
           Variable = summ.lst$Variable[i])
  summ2.tbl = bind_rows(summ2.tbl, i.tbl)
}

write_csv(summ2.tbl, 
          file = file.path(procDataDir, "PSD_Tukey_Comparision_pvalues.csv")) 
save_table(summ.tbl, procDataDir, "PSD_Tukey_Comparision_grouped")
