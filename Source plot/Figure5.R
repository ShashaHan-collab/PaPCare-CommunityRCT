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
# 设置工作目录
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
                              radial.labels = NA,label.prop=c(1.2,1.2,1.1,1.2,1.2,1.1)) # 隐藏径向轴标签
  
  n_labels <- length(c(1, 2, 3, 4, 5, NA))
  angles <- rep(0,6) # Adjust angles
  for (i in seq_along(angles)) {
    text_pos <-c(0,i-1)
    text(text_pos[2], text_pos[1], labels = c(1, 2, 3, 4, 5, NA)[i], col = "darkgray")
  }
  # 获取最大半径
  max_radius <- 5
  
  # 使用 lines() 函数画加粗的最外圈
  angles <- seq(0, 2 * pi, length.out = 100)
  x <- max_radius * cos(angles)
  y <- max_radius * sin(angles)
  
  # 绘制加粗的圆
  lines(x, y, lwd=2)  # 调整 lwd 以达到所需的粗细
  
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
# 设置边距为零
par(mar = c(0, 0, 0, 0))  # 下、左、上、右的边距全设置为 0
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
  
  # 使用 lines() 函数画加粗的最外圈
  angles <- seq(0, 2 * pi, length.out = 100)
  x <- max_radius * cos(angles)
  y <- max_radius * sin(angles)
  
  # 绘制加粗的圆
  lines(x, y, lwd=2)  # 调整 lwd 以达到所需的粗细
  
  
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
# 设置边距为零
par(mar = c(0, 0, 0, 0))  # 下、左、上、右的边距全设置为 0
radar(summary_data_5a2)
dev.off()

diagnosis<-percentage%>%filter(Variable=="diagnosis")
history_taking<-percentage%>%filter(Variable=="history taking")
test_ordering<-percentage%>%filter(Variable=="test ordering")
management<-percentage%>%filter(Variable=="management")
D=diagnosis
bar<-function(D){
  size=1
  label<-c(
    'Very\nunfavorable','Unfavorable',
    "Neutral",'Favorable',
    'Very\nfavorable'
  )
  # }
  if(unique(D$Variable)=="diagnosis") xlabel<-'Diagnosis'
  if(unique(D$Variable)=="history taking") xlabel<-'History taking'
  if(unique(D$Variable)=="test ordering") xlabel<-'Test ordering'
  if(unique(D$Variable)=="management") xlabel<-'Long term disease management'
  D$group<-factor(D$group,levels = c( "Telemedicine", "Usual care" ,"Consultation−only","E−learning plus" ))
  ggplot(D, aes(x = as.factor(Value), y = Percentage, fill = group)) +
    geom_col(position = "dodge", alpha = 0.7) +  # 堆叠柱状图
    scale_x_discrete(labels = label) +  # 自定义x轴标签
    labs(
      x = xlabel, y = "Proportions  (%)"
    ) +
    #ylim(0, 300) +
    scale_fill_manual(values = c('#b0a5b8',"#bca480","#A8CBDF","#B54764")) +
    theme_minimal() +  # 简洁主题
    theme(legend.position = "none",panel.grid.major = element_blank(), # 去除主网格线
          panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),  # 居中x轴标签
          plot.title = element_markdown(hjust = 0.5, face = "bold")  # 渲染Markdown格式标题
    )+
    #labs(title = "", x = "", y = '')+
    scale_y_continuous(limits = c(0,60)) +
    #scale_x_discrete(labels = c("consultation\n only", "E-learning\n plus"))+
    geom_segment(aes(x =1, xend = 5, y = -Inf, yend = -Inf), color = "darkgray",linewidth=size) +
    geom_segment(aes(y = 0, yend = 60, x = -Inf, xend = -Inf), color = "darkgray",linewidth=size)+
    theme(
      legend.text = element_text(size = 10),
      axis.text.x = element_text(size = 11, color = "black"),
      axis.ticks.x = element_line(color = "darkgray"), 
      axis.title.x = element_text(size = 12, color = "black", face = "bold"),
      axis.text.y = element_text(size = 11, color = "black"),
      axis.ticks.y = element_line(color = "darkgray"), 
      axis.title.y = element_text(size = 12, color = "black", face = "bold"),
      axis.ticks.length = unit(0.25, "cm")
    )
}
bar2<-function(D){
  size=1
  label<-c(
    'Very\nunfavorable','Unfavorable',
    "Neutral",'Favorable',
    'Very\nfavorable'
  )
  if(unique(D$Variable)=="diagnosis") xlabel<-'Diagnosis'
  if(unique(D$Variable)=="history taking") xlabel<-'History taking'
  if(unique(D$Variable)=="test ordering") xlabel<-'Test ordering'
  if(unique(D$Variable)=="management") xlabel<-'Long term disease management'
  D$group<-factor(D$group,levels = c( "Telemedicine", "Usual care" ,"Consultation−only","E−learning plus" ))
  ggplot(D, aes(x = as.factor(Value), y = Percentage, fill = group)) +
    geom_col(position = "dodge", alpha = 0.7) +  # 堆叠柱状图
    scale_x_discrete(labels = label) +  # 自定义x轴标签
    labs(
      x = xlabel, y = "Proportions  (%)"
    ) +
    #ylim(0, 300) +
    scale_fill_manual(values = c('#b0a5b8',"#bca480","#A8CBDF","#B54764")) +
    theme_minimal() +  # 简洁主题
    theme(legend.position = "none",panel.grid.major = element_blank(), # 去除主网格线
          panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),  # 居中x轴标签
          plot.title = element_markdown(hjust = 0.5, face = "bold")  # 渲染Markdown格式标题
    )+
    #labs(title = "", x = "", y = '')+
    scale_y_continuous(limits = c(0,60)) +
    #scale_x_discrete(labels = c("consultation\n only", "E-learning\n plus"))+
    geom_segment(aes(x =1, xend = 5, y = -Inf, yend = -Inf), color = "darkgray",linewidth=size) +
    geom_segment(aes(y = 0, yend = 60, x = -Inf, xend = -Inf), color = "darkgray",linewidth=size)+
    theme(
      legend.position =c(0.5,0.975),
      legend.title = element_blank(),
      legend.direction = "horizontal", 
      legend.text = element_text(size = 10),
      axis.text.x = element_text(size = 11, color = "black"),
      axis.ticks.x = element_line(color = "darkgray"), 
      axis.title.x = element_text(size = 12, color = "black", face = "bold"),
      axis.text.y = element_text(size = 11, color = "black"),
      axis.ticks.y = element_line(color = "darkgray"), 
      axis.title.y = element_text(size = 12, color = "black", face = "bold"),
      axis.ticks.length = unit(0.25, "cm")
    )
}

p_2<-bar(diagnosis)
p_1<-bar2(history_taking)
p_3<-bar(test_ordering)
p_4<-bar(management)


p<-plot_grid(p_1,p_3,p_2,p_4,nrow=2,labels='')
ggsave("github/fig_5b_r.pdf", plot = p, width = 12, height = 4)
# group_colors<-c("#B54764","#A8CBDF","#bca480",'#b0a5b8')
# p<-ggplot(dat_extracted, aes(x = variable, y = mean, fill = group)) +
#   geom_bar(stat = "identity", position = position_dodge(width = 0.9), alpha = 0.7) + # 调整位置参数
#   geom_errorbar(aes(ymin = mean - 0.5 * sd, ymax = mean + 0.5 * sd), alpha = 0.7,
#                 width = 0, linewidth = 0.7, position = position_dodge(width = 0.9)) + # 同步位置调整
#   labs(title = "",
#        x = "",
#        y = "Likert scale of score") +
#   theme_minimal() +
#   geom_segment(aes(y = 0, yend = 5, x = -Inf, xend = -Inf), color = "darkgray", linewidth = 0.5) +
#   scale_y_continuous(limits = c(0, 5), breaks = c(0, 1, 2, 3, 4, 5)) +
#   scale_fill_manual(values = group_colors) +
#   theme(legend.position = 'top',
#         legend.title = element_blank(),
#         legend.text = element_text(size = 15),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         axis.ticks.y = element_line(color = "darkgray"),
#         axis.title.y = element_text(size = 20, color = "black", face = "bold"),
#         axis.ticks.length = unit(0.25, "cm"),
#         axis.text.y = element_text(size = 18),
#         plot.margin = unit(c(0, 0, 0, 0), "cm"),
#         axis.text.x = element_text(size = 16, margin = margin(t = -10))
#   )
ggsave(filename = "github/figure_5b.pdf", plot = p, width = 15, height = 6)

