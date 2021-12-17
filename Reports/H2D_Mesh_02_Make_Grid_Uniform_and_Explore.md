---
title: "Explore Hydrus-2D Mesh Data"
author: "Samuel Araya"
date: 'April 15, 2021'
output:
   html_document:
     toc: true
     keep_md: true
---


```r
## Libraries
library(tidyverse)
library(viridis) # color for ggplot
library(scales) # to access break formatting functions for ggplot
## Data Directories
wDir <- getwd()
parent_dir = file.path(wDir,"Hydrus_Simulations_Out")
dataDir <- file.path(wDir,"Data")
pdataDir <- file.path(wDir, "Data_Processed")
outDir <- file.path(wDir,"R_Print")
## Function to save tables as RDS and CSV at one time:
save_table <- function(mytable, mypath, myfilename, rds_only = FALSE){
  csv_fullpath = file.path(mypath, paste0(myfilename, ".csv") )
  rds_fullpath = file.path(mypath, paste0(myfilename, ".rds") )
  
  if(!rds_only){
    write_csv(mytable, path = csv_fullpath)
  }
  write_rds(mytable, path = rds_fullpath)
  
}
## Function to save plots as pdf and png at one time:
save_plot <- function(myplot, myfilename, width, height,
                      mypath = outDir){
  ggsave(plot = myplot, width = width, height = height,
         filename = file.path(mypath, paste0(myfilename,".pdf")))
  
  ggsave(plot = myplot, width = width, height = height,
         filename = file.path(mypath, paste0(myfilename,".png")))
  
}
```

## Read in Regular Mesh Data
 Supply a 'look-up' table for variables


```r
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
```

Read in Regular Mesh Data


```r
# # i.mesh.dt is interpolated by 0.5 cm
# i.mesh.dt = read_rds( file = file.path(pdataDir ,"Interpolated_Mesh_Mean_th_h_Treatment.rds") )
```

Data is organized into a long table with values defined for x and y combinations.
The x and y values have a resolution of 0.25 cm.


```r
# head(i.mesh.dt)
## data interpolated by 0.25 cm
mesh.dt = read_rds( file = file.path(pdataDir ,"Selected_Time_Interpolated_Mesh_th_h_by_Treatment.rds") )
head(mesh.dt) %>%
  kableExtra::kbl() %>%
  kableExtra::kable_classic("hover", full_width = F)
```

<table class=" lightable-classic lightable-hover" style='font-family: "Arial Narrow", "Source Sans Pro", sans-serif; width: auto !important; margin-left: auto; margin-right: auto;'>
 <thead>
  <tr>
   <th style="text-align:right;"> Time </th>
   <th style="text-align:left;"> Trt </th>
   <th style="text-align:right;"> x </th>
   <th style="text-align:right;"> y </th>
   <th style="text-align:right;"> Depth </th>
   <th style="text-align:right;"> h_mean </th>
   <th style="text-align:right;"> h_sd </th>
   <th style="text-align:right;"> th_mean </th>
   <th style="text-align:right;"> th_sd </th>
   <th style="text-align:right;"> n </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> ST-NO </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> -100.00 </td>
   <td style="text-align:right;"> -3398.631 </td>
   <td style="text-align:right;"> 191.3602 </td>
   <td style="text-align:right;"> 0.1493100 </td>
   <td style="text-align:right;"> 0.0018861 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> ST-NO </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> -99.75 </td>
   <td style="text-align:right;"> -3398.709 </td>
   <td style="text-align:right;"> 191.3938 </td>
   <td style="text-align:right;"> 0.1493092 </td>
   <td style="text-align:right;"> 0.0018864 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> ST-NO </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> -99.50 </td>
   <td style="text-align:right;"> -3398.786 </td>
   <td style="text-align:right;"> 191.4275 </td>
   <td style="text-align:right;"> 0.1493084 </td>
   <td style="text-align:right;"> 0.0018867 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> ST-NO </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> -99.25 </td>
   <td style="text-align:right;"> -3398.864 </td>
   <td style="text-align:right;"> 191.4611 </td>
   <td style="text-align:right;"> 0.1493077 </td>
   <td style="text-align:right;"> 0.0018871 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> ST-NO </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> -99.00 </td>
   <td style="text-align:right;"> -3398.941 </td>
   <td style="text-align:right;"> 191.4948 </td>
   <td style="text-align:right;"> 0.1493069 </td>
   <td style="text-align:right;"> 0.0018874 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> ST-NO </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> -98.75 </td>
   <td style="text-align:right;"> -3399.018 </td>
   <td style="text-align:right;"> 191.5284 </td>
   <td style="text-align:right;"> 0.1493061 </td>
   <td style="text-align:right;"> 0.0018877 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
</tbody>
</table>

## Mesh Plots


```r
## Basic Plot setup
theme_bw_plus <- function(base_size = 12, base_family = ""){
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      panel.grid = element_blank(),
      strip.background = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA),
      plot.title = element_text(hjust = 0.5)
      
    )
}
notill_color <- c("#000000", # Black
                  "#E69F00", # Orange 
                  "#614BC7", # Purple
                  "#009E73" ) # Green

notill_line <- c("solid",
                 "longdash",
                 "twodash",
                 "dotdash" )
th.label = bquote(theta~' ['*cm~cm^{-1}*']' ) 
h.label = bquote(h*' [cm]') 

# Subset 5 time steps
sub.mesh.dt = mesh.dt %>% 
  dplyr::filter(Time %in% c(0, 8, 48, 72) ) %>% 
  dplyr::mutate(Time_lbl = ifelse(Time==0, "Irrigation Start", 
                                  ifelse(Time==8, "0 hours", paste(Time, "hours") )))
```

### Plot of $\theta$


```r
mp = ggplot(data = sub.mesh.dt) +
  geom_raster(aes(x = x*0.25, y = Depth, fill = th_mean)) +
  # geom_hline(yintercept = c(-20, -30, -60), 
  #            linetype = "dotted", col = "grey", size  =0.5)+
  facet_grid(fct_reorder(Time_lbl, Time)~Trt)+
  labs(
    y = "Depth [cm]",
    x = "Radius [cm]")+
  theme_bw_plus() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_viridis(direction = -1,
                     name=th.label )+
  coord_fixed(ratio = 1)
  

mp  
```

![](H2D_Mesh_01_Make_Grid_Uniform_and_Explore_files/figure-html/th_mesh_Plot-1.png)<!-- -->

```r
mp_50 = mp +  
  scale_y_continuous(limit = c(-50, 0), expand = c(0, 0))

mp_50
```

![](H2D_Mesh_01_Make_Grid_Uniform_and_Explore_files/figure-html/th_mesh_Plot_50-1.png)<!-- -->

```r
save_plot(mp_50, "Dynamic_2HD_th_Mesh", 6,10)
```

### Plot of $h$


```r
mph = ggplot(data = sub.mesh.dt) +
  geom_raster(aes(x = x*0.25, y = Depth, fill = h_mean)) +
  # geom_hline(yintercept = c(-20, -30, -60), 
  #            linetype = "dotted", col = "grey", size  =0.5)+
  facet_grid(fct_reorder(Time_lbl, Time)~Trt)+
  labs(
    y = "Depth [cm]",
    x = "Radius [cm]")+
  theme_bw_plus() +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_viridis(direction = -1,
                     name= h.label )+
  coord_fixed(ratio = 1)


mph
```

![](H2D_Mesh_01_Make_Grid_Uniform_and_Explore_files/figure-html/h_mesh_Plot-1.png)<!-- -->

```r
mph_50 = mph +  
  scale_y_continuous(limit = c(-50, 0), expand = c(0, 0))

mph_50
```

![](H2D_Mesh_01_Make_Grid_Uniform_and_Explore_files/figure-html/h_mesh_Plot_50-1.png)<!-- -->

```r
save_plot(mph_50, "Dynamic_2HD_h_Mesh", 6,10)
```

## Aggregate Mesh into 1-D by Depth 

To average the axisymmetric mesh measurements into one-dimension by depth, 
we need to weigh each horizontal measurement (along x-axis) by the 
circumference it represents in the 3-D cylinder, 
the weight is a function of how far away it is from the center.

To do this we weigh each measurement by its distance from the center (i.e.,
by its x-value) as:

$$\theta(y) = \frac{2 \pi \int_{0}^{R}  x \theta(x) \,dx }{\pi {R}^{2}}$$

$$\Rightarrow \theta(y) = \frac{2}{R^2} \int_{0}^{R}  x \theta(x) \,dx $$

where $\theta$ is the water content, 
$x$ and $y$ are the horizontal (radial) and vertical (depth) coordinates in cm,
$R$ is the radius of the domain (i.e., 18 cm) or any smaller number over which
the horizontal aggregation is required.

Application of equation in R:


```r
aggregate_by_x = function(x, R_cm, value, x_resolution = 0.25){
  ## Function to aggregate axisymmetric mesh values
  #
  ## Variables:
    # x: the x coordinate
    # R_cm: the Radius of the domain in cm
    # value: the value of the point (either theta or h)
    # x_resolution: the resolution of the interpolation in x axis
  ## Returns
    # mean value (either theta or h) that is weighted by the perimeter
  x_cm = x * x_resolution
  value_y = (2 /(R_cm^2))  * sum(x_cm * value)
  ## !*** divide by resolution of x axis ***
  value_y = value_y * x_resolution
  return(value_y)
}

sub.dt = mesh.dt %>% 
  dplyr::filter(Time %in% c(8, 24, 48, 72) ) %>% #8 is Time after end of irrigation
  # dplyr::filter(Time %in% c(0, 24, 48, 72)) %>% 
  dplyr::group_by(Trt, Time, Depth) %>% 
  dplyr::summarise(h_mean = aggregate_by_x(x,
                                           R_cm = 18,
                                           value = h_mean),
                   th_mean = aggregate_by_x(x,
                                            R_cm = 18,
                                            value = th_mean)) %>% 
  # label time for plot
  dplyr::mutate(Time_lbl = ifelse(Time==8, "0 hours", paste(Time, "hours") ))
```

## Plot Variables by depth

### Profile plots of $\theta$


```r
p = ggplot(sub.dt, aes(x = th_mean, y = Depth, group = Trt)) +
  geom_hline(yintercept = c(-20, -30, -60), 
             linetype = "dotted", col = "grey", size  =0.5)+
  # annotate("point", x = 0.14, y = -10, shape = 21, size = 4, fill = "grey80",
  #           color = "grey60")+
  geom_path(aes(group=Trt, 
                linetype=Trt,
                color = Trt), size = 0.75)+
  facet_grid(~as.factor(Time_lbl))+
  labs(
    y = "Depth [cm]",
    x = th.label)+
  scale_color_manual(values = notill_color, 
                     name="Treatment") +
  scale_linetype_manual(values= notill_line, 
                        name="Treatment") +
  theme_bw_plus() +
  theme(panel.grid = element_blank(), 
        #legend.position=c(0.85, 0.25),
        legend.title=element_blank(),
        legend.background = element_rect(size=0.5,
                                         color = "black",
                                         linetype="solid"),
        legend.position = c(0.99, 0.01),
        legend.justification = c("right", "bottom"),
        legend.margin =  margin(0,2,1,0.5),
        legend.key.height = unit(0.45, "cm"))+
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_continuous(expand = c(0, 0)) 

p
```

![](H2D_Mesh_01_Make_Grid_Uniform_and_Explore_files/figure-html/theta_Plot-1.png)<!-- -->

```r
save_plot(p, "Dynamic_2HD_th_by_Depth_weighted", 7,5)
```

```r
p_50 = p +  
  scale_y_continuous(limit = c(-50, 0), expand = c(0, 0))

p_50
```

![](H2D_Mesh_01_Make_Grid_Uniform_and_Explore_files/figure-html/theta_Plot_50-1.png)<!-- -->

```r
save_plot(p_50, "Dynamic_2HD_th_by_Depth_weighted_top_50", 7,4)
```

### Profile Plots of $h$



```r
ph = ggplot(sub.dt, aes(x = (h_mean*-1), y = Depth, group = Trt)) +
  geom_hline(yintercept = c(-20, -30, -60), 
             linetype = "dotted", col = "grey", size  =0.5)+
  # annotate("point", x = 0.14, y = -10, shape = 21, size = 4, fill = "grey80",
  #          color = "grey60")+
  geom_path(aes(group=Trt, 
                linetype=Trt,
                color = Trt), size = 0.75)+
  facet_grid(~as.factor(Time_lbl), 
             scales = "free",
             )+
  labs(
    y = "Depth [cm]",
    x = h.label)+
  scale_color_manual(values = notill_color, 
                     name="Treatment") +
  scale_linetype_manual(values= notill_line, 
                        name="Treatment") +
  theme_bw_plus() +
  theme(panel.grid = element_blank(), 
        #legend.position=c(0.85, 0.25),
        legend.title=element_blank(),
        legend.background = element_rect(size=0.5,
                                         color = "black",
                                         linetype="solid"),
        legend.position = c(0.91, 0.01),
        legend.justification = c("right", "bottom"),
        legend.margin =  margin(0,2,1,0.5),
        legend.key.height = unit(0.45, "cm")#,
        #axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
        )+
  #scale_x_continuous(expand = c(0.01, 0)) +
  scale_x_log10() + #breaks = trans_breaks("log10", function(x) 10^x),
                #labels = trans_format("log10", scales::math_format(10^.x))) +
  annotation_logticks(colour = "gray", sides = "b")+
  scale_y_continuous(expand = c(0, 0)) 

ph
```

![](H2D_Mesh_01_Make_Grid_Uniform_and_Explore_files/figure-html/h_Plot-1.png)<!-- -->

```r
save_plot(ph, "Dynamic_2HD_h_by_Depth_weighted", 7,5.5)
```

```r
ph_50 = ph +  
  scale_y_continuous(limit = c(-50, 0), expand = c(0, 0)) +
  theme(legend.position = c(0.98, 0.01))

ph_50
```

![](H2D_Mesh_01_Make_Grid_Uniform_and_Explore_files/figure-html/h_Plot_50-1.png)<!-- -->

```r
save_plot(ph_50, "Dynamic_2HD_h_by_Depth_weighted_top_50", 7,4.5)
```

