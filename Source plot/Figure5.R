rm(list = ls())
library(fmsb)
library(ggplot2)
library(tidyverse)
library(ggtext)
library(gridExtra)
library(cowplot)
library(ggpattern)
library(plotrix)
library(readxl)
library(dplyr)
library(reshape2)
library(jsonlite)
library(openxlsx)

# load data
load('github/source_data_5a.RData')
load('github/source_data_5b.RData')
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
  color <- c("#b5bca3", "#bca480")
  lwd = 3
  
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
  
  legend(x=0.725, y=6.75, legend=c( "With pediatric consultations","Without pediatric consultations"), col=c(color[2],color[1]), pch=c(21, 21),
         lty=1, cex=1,pt.bg = "white", bty = "n" )
}
pdf("github/figure_5a1.pdf", width = 6, height = 6)

par(mar = c(0, 0, 0, 0))  
radar(summary_data_5a1)
dev.off()

radar <- function(summary_data) {
  
  ppp <- matrix(summary_data$mean, nrow=1, ncol=6)
  colnames(ppp) <- c('Attention', 'Awareness', 'Listenability', 'Conciseness', 'Integrity', 'Empathy')
  ppp <- ppp[, c(2, 1, 3:6)]
  ppp<-ppp[6:1]
  kl <- c(0, pi/3, 2*pi/3, pi, 4*pi/3, 5*pi/3)
  start <- 5*pi/6 # starting value for plotting points angularly
  angle_offset_factor <- pi / 40 # small angle offset
  rad_low_lim <- 1 # used in plot limits
  color <- c('#b0a5b8')
  lwd = 3
  ppp<-data.frame(t(ppp))
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
                              radial.labels = NA,label.prop=c(1.2,1.2,1.1,1.2,1.2,1.1)) # 隐藏径向轴标签
  
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
  
}
pdf("github/figure_5a2.pdf", width = 6, height = 6)
par(mar = c(0, 0, 0, 0))  
radar(summary_data_5a2)
dev.off()

group_colors<-c("#B54764","#A8CBDF","#bca480",'#b0a5b8')
p<-ggplot(dat_extracted, aes(x = variable, y = mean, fill = group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), alpha = 0.7) 
  geom_errorbar(aes(ymin = mean - 0.5 * sd, ymax = mean + 0.5 * sd), alpha = 0.7,
                width = 0, linewidth = 0.7, position = position_dodge(width = 0.9)) 
  labs(title = "",
       x = "",
       y = "Likert scale of score") +
  theme_minimal() +
  geom_segment(aes(y = 0, yend = 5, x = -Inf, xend = -Inf), color = "darkgray", linewidth = 0.5) +
  scale_y_continuous(limits = c(0, 5), breaks = c(0, 1, 2, 3, 4, 5)) +
  scale_fill_manual(values = group_colors) +
  theme(legend.position = 'top',
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_line(color = "darkgray"),
        axis.title.y = element_text(size = 20, color = "black", face = "bold"),
        axis.ticks.length = unit(0.25, "cm"),
        axis.text.y = element_text(size = 18),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        axis.text.x = element_text(size = 16, margin = margin(t = -10))
  )
ggsave(filename = "github/figure_5b.pdf", plot = p, width = 15, height = 6)


