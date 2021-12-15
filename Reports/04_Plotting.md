---
title: "Plots for Publication"
author: "Samuel Araya"
date: 'December 09, 2021'
output:
   html_document:
     toc: true
     keep_md: true
---
This script uses data exported from `02_Make_Lab_Data_Analysis_Ready.R` and
`04_Plotting.R` script and generates publication quality plots.



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
library(ggpubr)
library(kableExtra)
library(bayestestR)
## Custom Functions
source("functions_custom.R")
ln.r_area_porosity.lbl = 'Porosity'
ln.r_area.lbl = 'Pore Volume Density'
Sr.diff.lbl = 'Mean Pore Volume Density'
#
```

## Import Data


```r
dt.seq = read_rds(file = file.path(procDataDir, "Interpolated_Extraploated_spline_K_theta_pF.rds"))
## Characteristic values with statistical tests
summ.tbl = read_rds(file = file.path(procDataDir, "LSD_Comparision_grouped_p_015.rds")) 
psdsumm.tbl = read_rds(file = file.path(procDataDir, "PSD_Tukey_Comparision_grouped.rds")) 
```

Format Tables for Plotting


```r
notill_levels =  c("ST-NO", "ST-CC", "CT-NO", "CT-CC")
notill_labels = c("ST-NO", "ST-CC", "NT-NO", "NT-CC")

dt.seq = dt.seq %>%
  dplyr::mutate(
    Trt = dplyr::recode_factor(
      Trt,
      "ST-NO" = "ST-NO",
      "ST-CC" = "ST-CC",
      "CT-NO" = "NT-NO",
      "CT-CC" = "NT-CC"
    )
  )
```

Add labeling column to table


```r
ksnorm.label = bquote(K[s]~' ['*cm~d^{-1}*']')
k100norm.label = bquote(K[100*cm]~' ['*cm~d^{-1}*']' )
wp.label = bquote(theta[PWP]~' ['*cm^3~cm^{-3}*']') 

summ.tbl = summ.tbl %>%
  mutate(
    Depth_Text = factor(
      Depth_ID,
      levels = c("T", "B"),
      labels = c(expression("0-5 cm"), expression("20-25 cm")) ,
      ordered = TRUE
    ),
    Trt = factor(Trt, levels =  notill_labels, ordered = TRUE),
    Variable.lbl = factor(
      Variable,
      levels = c(
        "BD",
        "Ks",
        "K100",
        "FC",
        "FC100",
        "WP",
        "PAW",
        "PAW100",
        "Porosity"
      ),
      labels = c(
        bd.label,
        ksnorm.label,
        k100norm.label,
        fc.label,
        fc100.label,
        wp.label,
        paw.label,
        paw100.label,
        por.label
      ) ,
      ordered = TRUE
    )
  )

p_namevec = c("BD", "Ks", "K100", "FC", "FC100", 
              "WP", "PAW", "PAW100", "Porosity")
```

## Plot Statistical Comparison
Bulk Density


```r
pc.bd = plot_stats_dodge(p_namevec[c(1)],
                         summ.tbl,
                         "Variable.lbl") +
  theme(legend.position = c(0.8, 0.2),
        legend.margin =  margin(0, 2, 1, 0.5))
pc.bd
```

![](I:\My Drive\NoTill\Araya_etal_2021_R\Reports\04_Plotting_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

Saturated Conductivity


```r
pc.k = plot_stats_dodge(p_namevec[c(2)],
                        summ.tbl, "Variable.lbl", logy = T) +
  theme(legend.position = c(0.25, 0.1),
        legend.margin =  margin(0, 2, 1, 0.5))
pc.k
```

![](I:\My Drive\NoTill\Araya_etal_2021_R\Reports\04_Plotting_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

Field Capacity


```r
pc.fc = plot_stats_dodge(p_namevec[c(4)],
                         summ.tbl,
                         "Variable.lbl") +
  #theme(legend.position = "None")
  theme(
    legend.position = c(0.8, 0.85),
    #legend.key.size = unit(0.5, "cm"),
    legend.margin =  margin(0, 2, 1, 0.5),
    legend.key.height = unit(0.45, "cm"),
    axis.text.x = element_blank()
  )
pc.fc
```

![](I:\My Drive\NoTill\Araya_etal_2021_R\Reports\04_Plotting_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

Plant Available Water Content


```r
pc.paw = plot_stats_dodge(p_namevec[c(7)],
                          summ.tbl,
                          "Variable.lbl") +
  theme(
    legend.position = c(0.8, 0.85),
    legend.margin =  margin(0, 2, 1, 0.5),
    legend.key.height = unit(0.45, "cm")
  )
pc.paw
```

![](I:\My Drive\NoTill\Araya_etal_2021_R\Reports\04_Plotting_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

Combine Plots into one
#+ fig.width=7, fig.height=7.5


```r
pc.bd = pc.bd + theme(legend.position = "None",
                      axis.text.x = element_blank())
pc.k = pc.k + theme(legend.position = "None")
pc.fc = pc.fc + theme(axis.text.x = element_blank())
pc.paw = pc.paw + theme(legend.position = "None")
p.comb <- ggpubr::ggarrange(pc.bd, pc.fc, pc.k,  pc.paw)

p.comb
```

![](I:\My Drive\NoTill\Araya_etal_2021_R\Reports\04_Plotting_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```r
ggsave(
  plot = p.comb,
  width = 7,
  height = 7.5,
  filename = file.path(plotDir, "plot_bd_k_fc_paw_LSD_p015.pdf")
)
```

## Plot Pore Size Distribution


```r
sem <- function(x){
  sd(x)/sqrt(length(x))
} 

dt.seq.m <- dt.seq %>%
  group_by(Trt, Depth_text, h, r) %>%
  select(Trt, Depth_text,h, r, theta, K, waterdepth, loss, Sr.diff) %>%
  summarise_each(list(mean = mean, sd = sd, se = sem))
```

PSD Grouped


```r
dt.marking = data.frame(x =    c(0.2, 0.2, 10, 10, 50, 50, 1000, 1000),
                        xend = c(0.1, 10, 0.2, 50, 10, 1000, 50, NA ))
gbw.psd2 = shp_gplot(dt.seq.m, 'r*2', 'Sr.diff_mean', 'Trt', 
                     '~Depth_text', logy = F, logx = TRUE,
                     psd.label, "Frequency")
gbw.psd2 = gbw.psd2 +
  geom_segment(data = dt.marking, aes(x = x ,y = 0, xend = xend , yend = 0), 
               arrow = arrow(length = unit(0.2,"cm")),
               lineend = "butt", linejoin = "bevel",
               size = 0.01) +
  geom_vline(aes(xintercept = 0.2), linetype = "dotted", color = "grey50")+
  geom_vline(aes(xintercept = 10), linetype = "dotted", color = "grey50")+
  geom_vline(aes(xintercept = 50), linetype = "dotted", color = "grey50")+
  geom_vline(aes(xintercept = 1000), linetype = "dotted", color = "grey50")+
  # geom_label()
  coord_cartesian(xlim = c(10^-1,10^3), expand = T)  +
  theme(legend.position=c(0.58, 0.82))

gbw.psd2
```

![](I:\My Drive\NoTill\Araya_etal_2021_R\Reports\04_Plotting_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

### Plot of Area Under the Curve $$S(ln(r))*dln(r)$$
ln.r_area_porosity.lbl = 'Porosity'


```r
ln.r_area.lbl = 'Pore Volume Density'
Sr.diff.lbl = 'Mean Pore Volume Density'
psdsumm.tbl = psdsumm.tbl%>%
  mutate(Trt = factor(Trt, levels =  notill_labels, ordered = TRUE),
         Variable.lbl = factor(Variable,
                               levels = c("ln.r_area_porosity","ln.r_area", "Sr.diff"),
                               labels = c(ln.r_area_porosity.lbl,ln.r_area.lbl, Sr.diff.lbl) ,
                               ordered = TRUE)
  )
```

```r
psd.summ2 = ggplot(dplyr::filter(psdsumm.tbl, Variable == "ln.r_area"), 
                   aes(x = psd_group, 
                       y = mean,
                       group = Trt)) +
  geom_col(aes(fill = Trt), alpha = 0.8,
           color = "black", size=0.2, 
           width = 0.4,
           position=position_dodge(width = 0.65))+
  geom_errorbar(aes(ymin = mean - stder,
                    ymax = mean + stder,
                    color = Trt), width=.2,
                position=position_dodge(width=0.65)) +
  geom_text(aes(y = (mean+stder), 
                label = groups), 
            vjust = -0.5, 
            colour = "black",
            position=position_dodge(width=0.65))+
  facet_grid(~Depth_text) +
  scale_fill_manual(values = notill_color) +
  scale_color_manual(values = notill_color) +
  scale_shape_manual(values = 21:24) +
  labs(
    y = 'Relative Frequency',
    x = bquote('Effective Pore Diameter Ranges ['*mu*m*']' ) )+
  theme_bw_plus()+
  scale_y_continuous(expand = expansion(mult = c(0,0.05)))+
  theme(legend.position=c(0.58, 0.82))+
  theme(strip.placement = "outside",
        legend.title=element_blank(),
        legend.background = element_rect(size=0.5,
                                         color = "black",
                                         linetype="solid"))
psd.summ2
```

![](I:\My Drive\NoTill\Araya_etal_2021_R\Reports\04_Plotting_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

Combine plots


```r
xp = ggpubr::ggarrange(gbw.psd2, psd.summ2, 
                       labels = c("(A)", "(B)"),
                       ncol = 1, nrow = 2)


ggsave(plot = xp, width = 8, height = 9.5,
       filename = file.path(plotDir, "PSD_Curve_Statistic_AUC.pdf"))
```

