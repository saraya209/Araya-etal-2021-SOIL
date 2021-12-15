---
title: "Make Laboratory Data Analysis Ready"
author: "Samuel Araya"
date: 'December 09, 2021'
output:
   html_document:
     toc: true
     keep_md: true
---
This script uses data exported from `01_Tidyup_Lab_Data.R` script and:

- Makes a linearly interpolated uniform water retention and conductivity tables
- The output tables are ready to be used as input (lookup talbe) in `HYDRUS` software
- Apply **smoothing function for retention curve and calculate the derivative for Pore Size Distribution analysis**
- Explore Data by Summary Plots.



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
## Custom Functions
source("functions_custom.R")
## Import Treatment-Plot look up table
dt.look = read.csv(file.path(dataDir, "Treatment_Plot_Lookup.csv"))
#
```

## Import Tables


```r
## Read summary 
dt <- read_rds(file = file.path(procDataDir, "HYPROP_Fit_bi_Summary.rds"))
## Conductivity
dt.mk <- read_rds(file = file.path(procDataDir, "HYPROP_Evaluation_K_pF.rds"))
dt.mfk <- read_rds(file = file.path(procDataDir, "HYPROP_Bi_Fit_K_pF.rds"))
## Retention
dt.mr <- read_rds(file = file.path(procDataDir, "HYPROP_Evaluation_WRC_pF.rds"))
dt.mfr <- read_rds(file = file.path(procDataDir, "HYPROP_Bi_Fit_WRC_pF.rds"))
# Dry
dt.dry <- read_rds(file = file.path(procDataDir, "HYPROP_Dry.rds"))
```


## Clean Up and Fix Table


```r
### new TREATMENT NAME AND LEVELS
notill_levels =  c("ST-NO", "ST-CC", "CT-NO", "CT-CC")
notill_labels = c("ST-NO", "ST-CC", "NT-NO", "NT-CC")
# Fix dt dry
## Remove 5T soil (incomplete measurement)
dt.dry = dt.dry %>%
  dplyr::left_join(dt, by = "Sample_ID") %>%
  dplyr::group_by(Sample_ID) %>%
  dplyr::filter(Sample_ID != '5T') %>%
  dplyr::mutate(
    Trt = paste(Tillage_Type.x, Winter_Cover.x, sep = "-"),
    Trt = factor(
      Trt,
      levels =  notill_levels,
      labels = notill_labels,
      ordered = TRUE
    ),
    Time_sec = as.numeric(Time - min(Time)) ,
    wt_water = Weight - (BD * 250) ,
    # BD * Volume = wt of soil
    wt_loss = wt_water - max(wt_water),
    loss = wt_loss / (pi * (4 ^ 2)),
    h = mean(c(pF_B, pF_T), na.rm = T)
  )

## Remove 5T soil (incomplete measurement) 
## Remove one outlier Ksat value from 29T.
## Remove 6 extra copies of Ks for 29T
#dim(dt.mk)
dt.mk = dt.mk %>%
  filter(Sample_ID != '5T') %>%
  filter(pF != -2 | Sample_ID != '29T') %>%
  distinct(pF, manual, Sample_ID, .keep_all = T) %>%
  mutate(
    Trt = paste(Tillage_Type, Winter_Cover, sep = "-"),
    Trt = factor(Trt, levels =  notill_levels, ordered = TRUE)
  )
#dim(dt.mk)

dt = dt %>%
  filter(Sample_ID != '5T') %>%
  mutate(
    Trt = paste(Tillage_Type, Winter_Cover, sep = "-"),
    Trt = factor(Trt, levels =  notill_levels, ordered = TRUE)
  )


#dim(dt.mr)
dt.mr = dt.mr %>%
  filter(Sample_ID != '5T') %>%
  mutate(
    Trt = paste(Tillage_Type, Winter_Cover, sep = "-"),
    Trt = factor(Trt, levels =  notill_levels, ordered = TRUE)
  )
#dim(dt.mr)

#dim(dt.mfk)
dt.mfk = dt.mfk %>%
  filter(Sample_ID != '5T') %>%
  mutate(
    Trt = paste(Tillage_Type, Winter_Cover, sep = "-"),
    Trt = factor(Trt, levels =  notill_levels, ordered = TRUE)
  )
#dim(dt.mfk)

#dim(dt.mfr)
dt.mfr = dt.mfr %>%
  filter(Sample_ID != '5T') %>%
  mutate(
    Trt = paste(Tillage_Type, Winter_Cover, sep = "-"),
    Trt = factor(Trt, levels =  notill_levels, ordered = TRUE)
  )
```


## Interpolate Measurements

Interpolate linearly to make pF spacing consistent.
at the right end, extrapolate linearly using `approxExtrap()` function.
Create a sequence of 201 pF values [-2,6].


```r
seq.pF1 = (seq(-2,2, by = 0.04))
seq.pF2 = (seq(2.04,6, by = 0.04))
```

Interpolated theta values (fraction)


```r
seq.theta = dt.mr%>%
  group_by(Sample_ID)%>%
  summarise(h = list(c(seq.pF1, seq.pF2)), 
            theta_approx = list(
              c(approx(x = pF, y = VWC, xout = seq.pF1, rule = 2)[[2]],
                approxExtrap(x = pF, y = VWC, xout = seq.pF2, rule = 2)[[2]]))) %>%
  unnest(cols = c(h, theta_approx))


## Interpolated K values (cm/day)
seq.K = dt.mk %>%
  group_by(Sample_ID) %>%
  summarise(h = list(c(seq.pF1, seq.pF2)),
            K_approx = list(c(
              approx(
                x = pF,
                y = logK_cmd,
                xout = seq.pF1,
                rule = 2
              )[[2]],
              approxExtrap(
                x = pF,
                y = logK_cmd,
                xout = seq.pF2,
                rule = 2
              )[[2]]
            ))) %>%
  unnest(cols = c(h, K_approx))


## Combine HC and WR values and append Plot description from lookup table
dt.seq = seq.theta %>%
  left_join(seq.K,  by = c("Sample_ID", "h")) %>%
  left_join(dt.look, by = "Sample_ID")

dt.pts = dt %>%
  select(Sample_ID, BD, Porosity, Ks, th_r, th_s) %>%
  mutate(Ks = log10(Ks))
```

## Add Several Derived Variable Columns
 
The `.format` columns are for use in `HYDRUS` software 
input tables. `HYDRUS` requires certain 
format (https://www.pc-progress.com/forum/viewtopic.php?t=2507):

- Write to 4 significant digits (e.g. `-3.7500e+01`)
- make table dimention to 100 rows, range log(h) [-2,6]
- set `iCap = 0`
hydraulic capacity (C) is not supplied, instead `HYDRUS` calculateS it 
as a slope of the retention curve.



```r
dt.seq = dt.seq %>%
  dplyr::group_by(Sample_ID) %>%
  dplyr::left_join(dt.pts, by = "Sample_ID") %>%
  dplyr::mutate(
    # Force interpolation to be monotone by cumulative minima
    theta = cummin(theta_approx),
    K = cummin(K_approx),
    
    # Format columns
    h.cm = 10 ^ h,
    ln.h.cm = log(h.cm),
    h.format = format(((10 ^ h) * -1),
                      digits = 5,
                      nsmall = 4,
                      scientific = TRUE
    ),
    theta.format = format(
      theta * 0.01,
      digits = 1,
      nsmall = 4,
      scientific = FALSE
    ),
    K.format = format(
      10 ^ K,
      digits = 5,
      nsmall = 4,
      scientific = TRUE
    ),
    Trt = paste(Tillage_Type, Winter_Cover, sep = "-"),
    Trt = factor(
      Trt,
      levels =  notill_levels,
      labels = notill_labels,
      ordered = TRUE
    ),
    
    # Transform variables
    theta.g = (theta) * 0.01 / BD,
    th_r.g = (th_r * 0.01) / BD,
    th_s.g = (th_s * 0.01) / BD,
    S = ((theta - th_r) / (th_s - th_r)) * 0.01,
    S.g = (theta.g - th_r.g) / (th_s.g - th_r.g),
    r = 1490 / h.cm,
    ln.r = log(r),
    
    ## Water loss
    waterdepth = (theta * 0.01 * 250) / (pi * (4 ^ 2)),
    loss = (waterdepth - waterdepth[h == -2]),
    
    ## Add spline fits
    K.spl = smooth.spline(h, K, spar = 0.75)$y,
    theta.spl = smooth.spline(h, theta, spar = 0.75)$y,
    loss.spl = smooth.spline(h, loss, spar = 0.75)$y,
    theta.g.spl = smooth.spline(ln.h.cm, theta.g, spar = 0.75)$y,
    S.g.spl = smooth.spline(ln.h.cm, S.g, spar = 0.75)$y,
    Sr.spl = smooth.spline(ln.r, S, spar = 0.75)$y,
    
    ## Calculate derivatives
    K.diff = (K.spl - lag(K.spl)) * -1,
    theta.diff = (theta.spl - lag(theta.spl)) * -1,
    loss.diff = (loss.spl - lag(loss.spl)) * -1,
    theta.g.diff = (theta.g.spl - lag(theta.g.spl)) * -1,
    S.g.diff = (S.g.spl - lag(S.g.spl)),
    Sr.diff = (Sr.spl - lag(Sr.spl)),
    
    # Add less smooth splines
    K.spl2 = smooth.spline(h, K, spar = 0.5)$y,
    theta.spl2 = smooth.spline(h, theta, spar = 0.5)$y,
    loss.spl2 = smooth.spline(h, loss, spar = 0.5)$y,
    theta.g.spl2 = smooth.spline(ln.h.cm, theta.g, spar = 0.5)$y,
    S.g.spl2 = smooth.spline(ln.h.cm, S.g, spar = 0.5)$y,
    Sr.spl2 = smooth.spline(ln.r, S, spar = 0.5)$y,
    
    ## Calculate derivatives
    K.diff2 = (K.spl2 - lag(K.spl2)) * -1,
    theta.diff2 = (theta.spl2 - lag(theta.spl2)) * -1,
    theta.diff2 = (theta.spl2 - lag(theta.spl2)) * -1,
    loss.diff2 = (loss.spl2 - lag(loss.spl2)) * -1,
    theta.g.diff2 = (theta.g.spl2 - lag(theta.g.spl2)) * -1,
    S.g.diff2 = (S.g.spl2 - lag(S.g.spl2)),
    Sr.diff2 = (Sr.spl2 - lag(Sr.spl2))
  )
```

### Export Interpolated and Formatted Table


```r
save_table(dt.seq, procDataDir, "Interpolated_Extraploated_spline_K_theta_pF")
```

## Explore Data



```r
## Plot labels
ks.title = 'Saturated Hydraulic Conductivity [cm/day]'
k.title = "Hydraulic Conductivities"
wrc.title = "Water Retention"
# K
k.label = bquote(K~' ['*cm~d^{-1}*']')
ks.label = bquote(K[s]~' ['*log[10](cm~d^{-1})*']')
k100.label = bquote(K(100~cm)~' ['*log[10](cm~d^{-1})*']' )
kdiff.label = bquote('Differential Conductivity ['*log[10](cm~d^{-1})~log[10](hPa)^{-1}*']' )
# theta
wrc.label = bquote(theta~' ['*cm^3~cm^{-3}*']' )
fc.label = bquote(theta[FC](0.3~MPa)~' ['*cm^3~cm^{-3}*']' )
fc100.label = bquote(theta[FC](0.1~MPa)~' ['*cm^3~cm^{-3}*']' )
pwp.label = bquote(theta[PWP](15~MPa)~' ['*cm^3~cm^{-3}*']' )
paw.label = bquote('PAW ('*theta[FC](0.3~MPa)~') ['*cm^3~cm^{-3}*']' )
paw100.label = bquote('PAW ('*theta[FC](0.1~MPa)~') ['*cm^3~cm^{-3}*']' )
wrcdiff.label = bquote('Differential Water Capacity ['*cm^3~cm^{-3}~log[10](hPa)^{-1}*']' )
psd.label = bquote('Effective Pore Diameter ['*mu*m*']' )
r.label = 'Pore Volume Density'
# physical
bd.label = bquote(rho[b]~' ['*g~cm^{-3}*']' ) 
por.label = bquote(phi~' ['*cm^3~cm^{-3}*']' ) 
pF.label = bquote('Suction ['*cm*']' )
h.label = bquote(h~' ['*cm*']' )
# general
trt.subtitle = "Grouped by Treatment"
```

### Smoothed Values by Treatment
Red curves are treatment means.


```r
pbw.ktrt = shp_bwplot(dt.seq, '10^h', '10^K', 'Sample_ID', 
                      'Depth_text~Trt', logy = T,
                      pF.label, k.label)

pbw.ktrt
```

![](I:\My Drive\NoTill\Araya_etal_2021_R\Reports\02_Make_Lab_Data_Analysis_Ready_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```r
ggsave(plot = pbw.ktrt, width = 7, height = 6,
       filename = file.path(plotDir, "K_by_treatment_with_mean.pdf"))
```

```r
pbw.thetatrt = shp_bwplot(dt.seq, '10^h', 'theta*0.01', 'Sample_ID', 
                          'Depth_text~Trt', logy = F,
                          pF.label, wrc.label)
pbw.thetatrt
```

![](I:\My Drive\NoTill\Araya_etal_2021_R\Reports\02_Make_Lab_Data_Analysis_Ready_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
ggsave(plot = pbw.thetatrt, width = 7, height = 6,
       filename = file.path(plotDir, "WRC_by_treatment_with_mean.pdf"))
```

### Smoothed Curves with Hyprop and KSAT Measured Points


```r
p.c = curve_with_points(dt.seq, dt.mk,
                        yval = 10^K,
                        yval2 = 10^logK_cmd,
                        title = "Hydraulic Conductivities", 
                        subtitle = "Smoothed with Measurement points", 
                        xlab = pF.label, 
                        ylab = k.label, 
                        logy = T)

p.c
```

![](I:\My Drive\NoTill\Araya_etal_2021_R\Reports\02_Make_Lab_Data_Analysis_Ready_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
ggsave(plot = p.c, width = 10, height = 10,
       filename = file.path(plotDir, "K_with_points.pdf"))
```

```r
p.wrc = curve_with_points(dt.seq, dt.mr, 
                          yval = theta*0.01,
                          yval2 = VWC*0.01, 
                        title = "Water Retention Curves", 
                        subtitle = "Smoothed with Measurement points", 
                        xlab = pF.label, 
                        ylab = wrc.label, 
                        logy = F)

p.wrc
```

![](I:\My Drive\NoTill\Araya_etal_2021_R\Reports\02_Make_Lab_Data_Analysis_Ready_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
ggsave(plot = p.wrc, width = 10, height = 10,
       filename = file.path(plotDir, "WRC_with_points.pdf"))
```

### Pore Size Distribution Plots
Plot with more smoothed WRC curve


```r
pbw.psd = shp_bwplot(dt.seq, 'r*2', 'Sr.diff*10', 'Sample_ID', 
                     'Depth_text~Trt', logy = F,
                     psd.label, r.label)
pbw.psd = pbw.psd+
  geom_vline(aes(xintercept = 0.5), linetype = "dotted", alpha = 0.80)+
  geom_vline(aes(xintercept = 50), linetype = "dotted", alpha = 0.80)+
  geom_vline(aes(xintercept = 500), linetype = "dotted", alpha = 0.80)+
  coord_cartesian(xlim = c(10^-1,10^3), expand = T)

pbw.psd
```

![](I:\My Drive\NoTill\Araya_etal_2021_R\Reports\02_Make_Lab_Data_Analysis_Ready_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
ggsave(plot = pbw.psd, width = 6, height = 4,
       filename = file.path(plotDir, "psd_by_treatment_with mean.pdf"))
```

Plot with less smooth WRC curve


```r
pbw.psd2 = shp_bwplot(dt.seq, 'r', 'Sr.diff2', 'Sample_ID', 
                      'Depth_text~Trt', logy = F,
                      psd.label, r.label)
pbw.psd2 = pbw.psd2+
  geom_vline(aes(xintercept = 0.5), linetype = "dotted", alpha = 0.80)+
  geom_vline(aes(xintercept = 50), linetype = "dotted", alpha = 0.80)+
  geom_vline(aes(xintercept = 500), linetype = "dotted", alpha = 0.80)+
  coord_cartesian(xlim = c(10^-1,10^3), expand = T)

pbw.psd2
```

![](I:\My Drive\NoTill\Araya_etal_2021_R\Reports\02_Make_Lab_Data_Analysis_Ready_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

```r
ggsave(plot = pbw.psd2, width = 9, height = 8,
       filename = file.path(plotDir, "psd2_by_treatment_mean.pdf"))
```

