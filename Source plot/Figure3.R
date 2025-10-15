rm(list = ls())

library(readxl)
library(fmsb)
library(dplyr)
library(jsonlite)
library(ggplot2)
library(reshape2)
library(tidyverse)
library(openxlsx)
library(plotrix)
library(ggtext)
library(gridExtra)
library(cowplot)
library(ggpattern)

# load data
load('github/source_data_3a.RData')
load('github/source_data_3b.RData')
load('github/source_data_3c.RData')
radar <- function(summary_data) {
  
  ppp <- matrix(summary_data$mean, nrow=2, ncol=6)
  colnames(ppp) <- c('Attention', 'Awareness', 'Listenability', 'Conciseness', 'Integrity', 'Empathy')
  rownames(ppp) <- c('F', 'T')
  ppp <- ppp[, c(2, 1, 3:6)]
  ppp<-ppp[,6:1]
  kl <- c(0, pi/3, 2*pi/3, pi, 4*pi/3, 5*pi/3)
  start <- 5*pi/6 # starting value for plotting points angularly
  angle_offset_factor <- pi / 40 # small angle offset
  rad_low_lim <- 1 # used in plot limits
  color <- c("#A8CBDF", "#B54764")
  lwd = 3
  
  # Plot without radial labels
  plot_rt_soa6 <- radial.plot(ppp[1,], rp.type="p",
                              radial.pos=kl-angle_offset_factor,
                              label.pos=kl,
                              start=start,
                              labels=colnames(ppp),
                              radial.lim=c(rad_low_lim, 6),
                              main="",
                              line.col=color[1],
                              lwd=lwd,
                              radial.labels = NA,label.prop=c(1.2,1.2,1.1,1.2,1.2,1.1)) 
  
  n_labels <- length(c(1, 2, 3, 4, 5, NA))
  angles <- rep(0,6) # Adjust angles
  for (i in seq_along(angles)) {
    text_pos <-c(0,i-1)
    text(text_pos[2], text_pos[1], labels = c(1, 2, 3, 4, 5, NA)[i], col = "darkgray")
  }
  max_radius <- 5
  
  angles <- seq(0, 2 * pi, length.out = 100)
  x <- max_radius * cos(angles)
  y <- max_radius * sin(angles)
  
  lines(x, y, lwd=2)  
  
  plot_rt_soa6 <- radial.plot(ppp[2,], rp.type="p",
                              radial.pos=kl+angle_offset_factor,
                              label.pos=kl,
                              start=start,
                              labels=colnames(ppp),
                              radial.lim=c(rad_low_lim, 6),
                              main="",
                              line.col=color[2],
                              add=TRUE,
                              lwd=lwd,
                              radlab=FALSE, # Disable radial labels
                              radial.labels = NA) # Hide radial axis labels
  
  # Generating standard error values for y
  error_ppp_y <- matrix(summary_data$se, nrow=2, ncol=6)
  
  bar_cols <- color # Colors for bars
  lwds <- c(lwd, lwd) # Line weights for bars
  pts_cols <- color # Colors for points
  pts_pch <- c(21, 21) # Filled circles for better visibility
  
  for (j in 1:2) {
    
    # Loop over the observations
    for (i in 1:ncol(ppp)) {
      
      # Apply random angle offsets to avoid overlap
      angle_offset <- ifelse(j == 1, -angle_offset_factor, angle_offset_factor)
      
      # Plotting the errors of the 'y' value
      lines(
        c(ppp[j, i] + error_ppp_y[j, i] - rad_low_lim, ppp[j, i] - error_ppp_y[j, i] - rad_low_lim) * cos(kl[i] + start + angle_offset),
        c(ppp[j, i] + error_ppp_y[j, i] - rad_low_lim, ppp[j, i] - error_ppp_y[j, i] - rad_low_lim) * sin(kl[i] + start + angle_offset),
        lwd=lwds[j],
        col=bar_cols[j]
      )
    }
  }
  
  for (j in 1:2) {
    # Loop over the observations
    for (i in 1:ncol(ppp)) {
      # Apply random angle offsets to avoid overlap
      angle_offset <- ifelse(j == 1, -angle_offset_factor, angle_offset_factor)
      posi<-ifelse(j==1,1,3)
      # Plotting points for the center with offset
      x_pos <- (ppp[j, i] - rad_low_lim) * cos(kl[i] + start + angle_offset)
      y_pos <- (ppp[j, i] - rad_low_lim) * sin(kl[i] + start + angle_offset)
      x_posi <- (ppp[j, i] - rad_low_lim+0.75) * cos(kl[i] + start + 2*angle_offset)
      y_posi <- (ppp[j, i] - rad_low_lim+0.75) * sin(kl[i] + start + 2*angle_offset)
      
      points(
        x_pos,
        y_pos,
        col=pts_cols[j],
        pch=pts_pch[j],
        bg = "white",# Use filled circle for points
        cex=1.5 # Increase point size if needed
      )
      
      # Adding text labels with values at each data point
      text(
        x_posi,
        y_posi,
        labels = sprintf("%.2f", ppp[j, i]), # Round to 2 decimal places for better display
        #pos =3, # Position text above the point
        cex = 1,
        bg = "white",# Size of text
        col = "black" # Text color
      )
    }
  }
  
  legend(x=2, y=6, legend=c( "E-learning plus","Consultation-only"), col=c(color[2],color[1]), pch=c(21, 21),
         lty=1, cex=0.8,pt.bg = "white", bty = "n" )
}
pdf("github/fig_3a.pdf", width = 6, height = 6)

par(mar = c(0, 0, 0, 0))  
radar(summary_data_3a)
dev.off()

summary_data<-summary_data_3b
useful<-summary_data[summary_data$variable=='useful',]
userfriendly<-summary_data[summary_data$variable=='user friendly',]
satisfied<-summary_data[summary_data$variable=='satisfied',]
recommended<-summary_data[summary_data$variable=='recommended',]
acceptable<-summary_data[summary_data$variable=='acceptable',]

bar<-function(D){
  size=1
  label<-c(
    paste0("extremely\nun", unique(D$variable)),
    paste0("un", unique(D$variable)),
    "neutral",
    paste0("", unique(D$variable)),
    paste0("extremely\n", unique(D$variable))
  )
  if(unique(D$variable)=="user friendly"){
    label<-c(
      paste0("extremely\n user unfriendly"),
      paste0("user unfriendly"),
      "neutral",
      paste0("", unique(D$variable)),
      paste0("extremely\n", unique(D$variable))
    )
  }
  if(unique(D$variable)=="user friendly") xlabel<-'User-friendliness'
  if(unique(D$variable)=="useful") xlabel<-'Usefulness'
  if(unique(D$variable)=="satisfied") xlabel<-'Satisfaction'
  if(unique(D$variable)=="recommended") xlabel<-'Recommendation'
  if(unique(D$variable)=="acceptable") xlabel<-'Acceptability'
  ggplot(D, aes(x = as.factor(value), y = counts, fill = Elearning)) +
    geom_col(position = "dodge") +  
    scale_x_discrete(labels = label) + 
    labs(
      x = xlabel, y = "No. of particpants", fill = "Elearning",
      title = "<span style='color:steelblue;'>Consultation-only</span> vs. <span style='color:gold;'>E-learning plus</span>"
    ) +
    #ylim(0, 300) +
    scale_fill_manual(values = c("TRUE" = "#FDD835", "FALSE" = "#1E88E5")) +
    theme_minimal() +  
    theme(legend.position = "none",panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),  
          plot.title = element_markdown(hjust = 0.5, face = "bold")  
          #axis.line.x = element_line(color = "darkgray"), 
          # axis.line.y = element_line(color = "black"), 
          # axis.ticks = element_line(color = "black"), 
          # axis.ticks.length = unit(0.25, "cm")
    )+
    #labs(title = "", x = "", y = '')+
    scale_y_continuous(limits = c(0,600)) +
    #scale_x_discrete(labels = c("consultation\n only", "E-learning\n plus"))+
    geom_segment(aes(x =1, xend = 5, y = -Inf, yend = -Inf), color = "darkgray",linewidth=size) +
    geom_segment(aes(y = 0, yend = 600, x = -Inf, xend = -Inf), color = "darkgray",linewidth=size)+
    theme(
      axis.text.x = element_text(size = 10, color = "black"),
      axis.ticks.x = element_line(color = "darkgray"), 
      axis.title.x = element_text(size = 10, color = "black", face = "bold"),
      axis.text.y = element_text(size = 10, color = "black"),
      axis.ticks.y = element_line(color = "darkgray"), 
      axis.title.y = element_text(size = 10, color = "black", face = "bold"),
      axis.ticks.length = unit(0.25, "cm")
    )
}

p_useful<-bar(useful)
p_userfriendly<-bar(userfriendly)
p_satisfied<-bar(satisfied)
p_recommended<-bar(recommended)
p_acceptable<-bar(acceptable)

p<-plot_grid(p_useful,p_satisfied,p_recommended,p_userfriendly,p_acceptable,nrow=5,labels='')
ggsave("github/fig_3b.pdf", plot = p, width = 6, height = 9)

gap<-function(summary_data){
  

  custom_colors <- c('#5495CFFF', '#F5AF4DFF', '#DB4743FF', '#7C873EFF','#FEF4D5FF')
  custom_patterns <- c("none", "circle")  
  
  n_vars <- length(levels(summary_data$variable))  
  p<-ggplot(summary_data, aes(x = variable, y = proportion, fill = variable, pattern = Elearning)) +
    geom_bar_pattern(
      stat = "identity",
      position = position_dodge(width = 0.9),
      color = "black",
      pattern_fill = "black",
      pattern_angle = 45,
      pattern_density = 0.1,
      pattern_spacing = 0.02,
      pattern_key_scale_factor = 0.6
    ) +
   
    geom_segment(
      x = -0.02,         
      xend = n_vars + 0.49,  
      y = 0.5,         
      yend = 0.5,
      linetype = "dashed",
      color = "gray",
      linewidth = 0.5
    ) +
    scale_pattern_manual(
      values = custom_patterns,
      name = "",
      labels = c("Consultation-only", "E-learning plus")
    ) +
    scale_fill_manual(
      values = custom_colors,
      guide = "none"
    ) +
    labs(title = "", x = "", y = "") +
    theme_minimal() +
    theme(
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12, margin = margin(t = -10)),
      legend.position = c(0.2, 0.89),  
      legend.title = element_text(size = 12, face = "bold"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks.y = element_line(color = "darkgray"),
      axis.title.y = element_text(size = 15, color = "black", face = "bold"),
      axis.ticks.length = unit(0.25, "cm")
    ) +
    guides(
      pattern = guide_legend(override.aes = list(fill = "white"))
    ) +
    geom_segment(aes(y = 0, yend = 0.8, x = -Inf, xend = -Inf), color = "darkgray", linewidth = 0.5) +
    scale_y_continuous(
      limits = c(0, 0.8),
      breaks = c(0, 0.2, 0.4, 0.6, 0.8),
      labels = c('0%', '20%', '40%', '60%', '80%')
    )
  return(p)
}
pc<-gap(summary_data_3c)
width=6

ggsave(filename = "github/fig_3c.pdf", plot = pc, width = width, height =width)
