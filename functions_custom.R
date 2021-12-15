## CUSTOM FUNCTIONS USED THROUGHT THE PROJECT
## Samuel Araya (2021)
## https://samuelna.netlify.app/

##
### Function to save tables as RDS and CSV at one time:
save_table <- function(mytable, mypath, myfilename){
  csv_fullpath = file.path(mypath, paste0(myfilename, ".csv") )
  rds_fullpath = file.path(mypath, paste0(myfilename, ".rds") )
  
  write_csv(mytable, file = csv_fullpath)
  write_rds(mytable, file = rds_fullpath)
  
}
### Function to save plots as pdf and png at one time:
save_plot <- function(myplot, myfilename, width, height,
                      mypath = outDir){
  ggsave(plot = myplot, width = width, height = height,
         filename = file.path(mypath, paste0(myfilename,".pdf")))
  
  ggsave(plot = myplot, width = width, height = height,
         filename = file.path(mypath, paste0(myfilename,".png")))
  
}

### Statistical functions
# Test normality
shapiro_wrap = function(x){
  shapiro.test(x)$p
}
# Levene’s test
levene_wrap = function(y, grp){
  leveneTest(y~grp)$`Pr(>F)`[1]
}
## Anova and Tukey's test table
compare_fun = function(y, trt_var, p = 0.15, letter_group = TRUE){
  dt.tbl = tibble(value = y, Trt = trt_var)
  #Do ANOVA
  model = aov(y~Trt, data = dt.tbl)
  #Do Tukey's LSD Test
  out = LSD.test(model, trt = "Trt", alpha = p, group = letter_group)
  #Format Output to neat tables
  if(letter_group){
    means.tbl = rownames_to_column(out$means)
    group.tbl = rownames_to_column(out$groups)[,-2]
    summ.tbl = left_join(means.tbl, group.tbl, by = "rowname")
    summ.tbl = summ.tbl%>%
      rename(Trt = rowname, mean = y) %>% 
      mutate(stder = std/sqrt(r))
  }else{
    p.tbl = rownames_to_column(out$comparison)
    summ.tbl = p.tbl%>%
      rename(Comparision = rowname)
  }
  
  return(summ.tbl)
}

### GGPLOT BASED FUNCTIONS FOR PLOTTING GRAPHS

### Plot theme

theme_bw_plus <- function(base_size = 12, base_family = ""){
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      panel.grid = element_blank(),
      strip.background = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA),
      plot.title = element_text(hjust = 0.5)
      
    )
}

## Color and line selection for treatments
notill_color <- c("#000000", # Black
                  "#E69F00", # Orange 
                  "#614BC7", # Purple
                  "#009E73"  # Green
)

notill_line <- c("solid",
                 "longdash",
                 "twodash",
                 "dotdash"
)

notill_line <- rev(notill_line)


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
fc.label = bquote(theta[FC]~' ['*cm^3~cm^{-3}*']' )
fc100.label = bquote(theta[FC](0.1~MPa)~' ['*cm^3~cm^{-3}*']' )
pwp.label = bquote(theta[PWP]~' ['*cm^3~cm^{-3}*']' )
paw.label = bquote('PAW ['*cm^3~cm^{-3}*']' )
paw100.label = bquote('PAW ('*theta[FC](0.1~MPa)~') ['*cm^3~cm^{-3}*']' )
wrcdiff.label = bquote('Differential Water Capacity ['*cm^3~cm^{-3}~log[10](hPa)^{-1}*']' )
psd.label = bquote('Effective Pore Diameter ['*mu*m*']' )
r.label = 'Pore Volume Density'
# physical
bd.label = bquote(rho[b]~' ['*g~cm^{-3}*']' ) 
por.label = bquote(phi~' ['*cm^3~cm^{-3}*']' ) 
pF.label = bquote('Suction ['*cm*']' )
h.label = bquote(h~' ['*cm*']' )
SD.label = bquote(S[D]~' [-]') 
alpha.label = bquote(alpha~' ['*cm^{-1}*']')
n.label = bquote(n~' [-]') 
## Plot Functions
shp_bwplot = function(data, xval, yval, grpval, facetval, logy = TRUE,
                      xlbl, ylbl){
  
  p.shp = ggplot(data, aes_string(x = xval, y = yval))+ 
    stat_summary(fun = "mean", geom="line", colour = "red", size = 0.75) +
    geom_path(aes_string(group=grpval), color = "black", alpha = 0.3 )+
    facet_grid(facetval) + 
    labs(x = xlbl,
         y = ylbl)+
    theme_bw_plus()
  
  # Log y
  if(logy){
    p.shp = p.shp+ 
      scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                    labels = trans_format("log10", scales::math_format(10^.x))) +
      scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                    labels = trans_format("log10", scales::math_format(10^.x)))+
      annotation_logticks(colour = "gray")#+
      #coord_cartesian(ylim = c(10^-3,10^4), xlim = c(10^-1.5,10^2.5), expand = T)
      #coord_cartesian(ylim = c(10^-5,10^4), xlim = c(10^-1.5,10^4), expand = T)
    
  }else{
    p.shp = p.shp+
      scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                    labels = trans_format("log10", scales::math_format(10^.x))) +
      annotation_logticks(colour = "gray", sides = "b")# +
      #coord_cartesian(xlim = c(10^-1,10^6), expand = T)
  }
  
  return(p.shp)
  
}


shp_gplot = function(data, xval, yval, 
                     grpval, facetval, 
                     logy = TRUE, logx = TRUE,
                     xlbl, ylbl){
  
  p.shp = ggplot(data, aes_string(x = xval, y = yval))+ 
    #geom_vline(aes(xintercept = 25), linetype = "dashed", alpha = 0.90)+
    geom_path(aes_string(group=grpval, 
                         linetype=grpval,
                         color = grpval), size = 0.75 )+
    facet_grid(facetval, scales = 'free' ) + 
    labs(
      x = xlbl,
      y = ylbl)+
    #scale_color_discrete(name=legendlbl)+
    #scale_color_viridis_d(name="Treatment") +
    scale_color_manual(values = notill_color, 
                       name="Treatment") +
    scale_linetype_manual(values= notill_line, 
                          name="Treatment") +
    #scale_linetype_discrete(name=legendlbl)+
    
    theme_bw_plus() +
    theme(panel.grid = element_blank(), 
          #legend.position=c(0.85, 0.25),
          legend.title=element_blank(),
          legend.background = element_rect(size=0.5,
                                           color = "black",
                                           linetype="solid"))
  
  # Log y
  if(logy & logx){
    p.shp = p.shp+ 
      scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                    labels = trans_format("log10", scales::math_format(10^.x))) +
      scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                    labels = trans_format("log10", scales::math_format(10^.x)))+
      annotation_logticks(colour = "gray")#+
      #coord_cartesian(ylim = c(10^-5,10^4), xlim = c(10^-1.5,10^4), expand = T)
    
  }else if(logx & !logy){
    p.shp = p.shp+ 
      scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                    labels = trans_format("log10", scales::math_format(10^.x))) +
      annotation_logticks(colour = "gray", sides = "b")# +
      #coord_cartesian(xlim = c(10^-1,10^6), expand = T)
  }
  
  return(p.shp)
  
}


plot_stats_dodge = function(x, p.tbl, facetval, logy = F){
  
  # subset to x variables
  p.tbl = p.tbl %>% 
    filter(Variable %in% x) %>% 
    dplyr::mutate(uci = mean + stder,
                  lci = mean - stder,
                  uci_text = (uci + abs(uci)*0.01) )
  if(logy){
    p.tbl = p.tbl %>% 
      dplyr::mutate(mean = 10^mean,
                    uci = 10^uci,
                    lci = 10^lci,
                    uci_text = ifelse(uci >10,
                                      uci*(1+0.1),
                                      uci))
  }
  
  p.summ = ggplot(p.tbl, aes(x = Depth_Text, 
                             y = mean,
                             group = Trt)) +
    geom_errorbar(aes(ymin = lci, 
                      ymax = uci,
                      color = Trt), width=.2,
                  position=position_dodge(width=0.5)) +
    geom_point(size = 2.25, aes(fill = Trt, shape = Trt),
               position=position_dodge(width=0.5))+
    geom_text(aes(y = uci_text, 
                  label = groups), 
              vjust = -0.05, 
              colour = "black",
              position=position_dodge(width=0.5))+
    scale_fill_manual(values = notill_color) +
    scale_color_manual(values = notill_color) +
    scale_shape_manual(values = 21:24) +
    labs(
      y = NULL,
      x = NULL)+
    facet_grid(facetval, scales = "free",  switch = "y",
               labeller = label_parsed
    ) +
    theme_bw_plus()+
    theme(strip.placement = "outside",
          legend.title=element_blank(),
          legend.background = element_rect(size=0.5,
                                           color = "black",
                                           linetype="solid"))
  # Log y
  if(logy){
    p.summ = p.summ + 
      scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                    labels = trans_format("log10", scales::math_format(10^.x)))+
      annotation_logticks(colour = "gray", sides = "l" )
  }
  
  return(p.summ)
}


curve_with_points = function(dt.seq, dt.m, yval, yval2, title, subtitle, xlab, ylab, logy = T){
  p = ggplot()+ 
    geom_point(data = dt.m, 
               aes(x = 10^pF, y = {{yval2}}, color = as.factor(Replicate)),
               shape = 3, alpha = 0.75)+
    geom_path(data = dt.seq, 
              aes(x = 10^h, y = {{yval}}, group = Sample_ID, color = as.factor(Replicate)) )+
    
    facet_grid(Depth_text~Tillage_Type+Winter_Cover) + 
    labs(title = title,
         subtitle = subtitle,
         x = xlab,
         y = ylab )+
    scale_color_discrete(name="Treatment \nReplicate ID")+
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", scales::math_format(10^.x))) +
    annotation_logticks(colour = "gray", sides = "b") +
    theme_bw() + theme(strip.background = element_rect( fill="white"),
                       panel.grid.minor = element_line(color = "grey95"))
  # Log y
  if(logy){
    p = p + 
      scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                    labels = trans_format("log10", scales::math_format(10^.x)),
                    limits = c(10^-5,10^5)) +
      annotation_logticks(colour = "gray", sides = "l" )
  }
  
  return(p)
}
