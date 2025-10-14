rm(list = ls())
library(plotrix)
library(readxl)
library(fmsb)
library(dplyr)
library(jsonlite)
library(ggplot2)
library(reshape2)
library(tidyverse)
library(openxlsx)
library(cowplot)
library(grid)
library(ggtext)
library(gridExtra)
load('github/source_data_4a.RData')
load('github/source_data_4b.RData')
colors <-c('#a7b9d7','#576fa0','#fadcb4','#e3b871','#dea3a2','#b57979','#cfcece','#9f9f9f')
group_colors <-colors[1:2]
fig4<-function(data,group_colors){
  if("Single" %in% unique(data$group)){ data$group=factor(data$group,levels = c('Single','Multiple'))}
  if("Urban" %in% unique(data$group)){ data$group=factor(data$group,levels = c('Urban','Rural'))}
  if("Female" %in% unique(data$group)){ data$group=factor(data$group,levels = c('Male','Female'))}
  size=1
  data$Metric <- factor(data$Metric, levels =  c("Awareness", "Attention","Listenability", "Conciseness", "Integrity", "Empathy"))
  # 绘制柱状图
  p<-ggplot(data, aes(x = Metric, y = Mean_Difference, fill = group)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.7)) + # 调整 width 使柱子部分重叠
    geom_errorbar(aes(ymin = Mean_Difference - SD_Difference, ymax = Mean_Difference + SD_Difference),
                  width = 0, linewidth = 0.7, position = position_dodge(width = 0.7)) + # 误差条与柱子对齐
    labs(title = "",
         x = "",
         y = "") +
    theme_minimal() +
    geom_segment(aes(y = 0, yend = 0.9, x = -Inf, xend = -Inf), color = "darkgray", linewidth = 0.5) +
    scale_y_continuous(limits = c(0, 0.9), breaks = c(0, 0.3, 0.6, 0.9)) +
    scale_fill_manual(values = group_colors)+
    theme(legend.position = c(0.8, 0.8),
          legend.title = element_blank(),
          legend.text = element_text(size = 15),
          panel.grid.major = element_blank(), # 去除主网格线
          panel.grid.minor = element_blank(),
          axis.ticks.y = element_line(color = "darkgray"), 
          axis.title.y = element_text(size = 20, color = "black", face = "bold"),
          axis.ticks.length = unit(0.25, "cm"),
          axis.text.y = element_text(size = 18))
  if("Single" %in% unique(data$group)){
    p<-p+ # 使用自定义颜色
      theme(
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 16, margin = margin(t = -10)), # 调整 margin 使文字离 x 轴更近
        
      )
  }else{
    p<-p+ # 使用自定义颜色
      theme(
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        axis.text.x = element_blank()# 调整 margin 使文字离 x 轴更
      )
  }
  
  return(p)
}
p1<-fig4(pb,colors[1:2])
p2<-fig4(pe,colors[3:4])
p3<-fig4(pd,colors[5:6])
p4<-fig4(pc,colors[7:8])

p<-plot_grid(p1,p2,p3,p4,nrow=4,rel_heights = c(1,1,1,1.2))
width=8
height =12
ggsave(filename = "github/fig_4a.pdf", plot = p, width = width, height =height)

custom_colors <- c("#D0D3A2FF", "#F0C6C3FF")
gap<-function(dat,label){
  data_long <- dat
  
  size=1
  
  p<-ggplot(data_long, aes(x = as.factor(Elearning), y = value, fill = Elearning)) +
    geom_violin(trim = FALSE, adjust = 1.5,show.legend =F,colour = NA, alpha = 0.5) +
    stat_summary(
      fun.data = ~data.frame(
        ymin = mean(.x) - 1*sd(.x),
        ymax = mean(.x) + 1*sd(.x),
        y = mean(.x)
      ),
      geom = "crossbar",
      width = 0.5,
    ) +
    #geom_boxplot(width = 0.8, outlier.shape = NA, color = "black", position = position_dodge(width = 0.75)) +
    #stat_summary(fun.data = boxplot_stats, geom = "text", position = position_dodge(width = 0.75), vjust = c(-1), size = 3.5, color = "black", hjust = 0.5) +
    scale_fill_manual(values = custom_colors) +
    theme_minimal() +
    theme(legend.position = "none",panel.grid.major = element_blank(), # 去除主网格线
          panel.grid.minor = element_blank(),
          #axis.line.x = element_line(color = "darkgray"), # 显示x轴线
          # axis.line.y = element_line(color = "black"), # 显示y轴线
          # axis.ticks = element_line(color = "black"), # 显示刻度线
          # axis.ticks.length = unit(0.25, "cm")
    )+
    labs(title = "", x = label, y = '')+
    scale_y_continuous(limits = c(-6,6),breaks = c(-4,-2,0,2,4)) +
    scale_x_discrete(labels = c("consultation\n only"))+
    geom_segment(aes(x =1, xend = 2, y = -Inf, yend = -Inf), color = "darkgray",size=size) +
    geom_segment(aes(y = -6, yend = 6, x = -Inf, xend = -Inf), color = "darkgray",size=size)+
    theme(
      axis.text.x =  element_blank(),
      axis.ticks.x = element_line(color = "darkgray"), 
      #axis.title.x = element_text(size = 15, color = "black", face = "bold"),
      axis.text.y = element_text(size = 15, color = "black"),
      axis.ticks.y = element_line(color = "darkgray"), 
      axis.title.x = element_text(size = 15, color = "black", face = "bold"),
      axis.title.y = element_text(size = 15, color = "black", face = "bold"),
      axis.ticks.length = unit(0.25, "cm"),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
    )
  
  return(p)
}
gap2<-function(dat,label){
  data_long <- dat
  # custom_colors <- c("#1b9e77", "#d95f02")
  size=1
  p<-ggplot(data_long, aes(x = as.factor(Elearning), y = value, fill = Elearning)) +
    geom_violin(trim = FALSE, adjust = 1.5,show.legend =F,colour = NA, alpha = 0.5) +
    stat_summary(
      fun.data = ~data.frame(
        ymin = mean(.x) - 1*sd(.x),
        ymax = mean(.x) + 1*sd(.x),
        y = mean(.x)
      ),
      geom = "crossbar",
      width = 0.5,
    ) +
    #geom_boxplot(width = 0.8, outlier.shape = NA, color = "black", position = position_dodge(width = 0.75)) +
    #stat_summary(fun.data = boxplot_stats, geom = "text", position = position_dodge(width = 0.75), vjust = c(-1), size = 3.5, color = "black", hjust = 0.5) +
    scale_fill_manual(values = custom_colors) +
    theme_minimal() +
    theme(legend.position = "none",panel.grid.major = element_blank(), # 去除主网格线
          panel.grid.minor = element_blank(),
          #axis.line.x = element_line(color = "darkgray"), # 显示x轴线
          # axis.line.y = element_line(color = "black"), # 显示y轴线
          # axis.ticks = element_line(color = "black"), # 显示刻度线
          # axis.ticks.length = unit(0.25, "cm")
    )+
    labs(title = "", x = label, y = '')+
    #scale_y_continuous(limits = c(-6,6),breaks = c(-4,-2,0,2,4)) +
    #scale_x_discrete(labels = c("consultation\n only"))+
    geom_segment(aes(x =1, xend = 2, y = -Inf, yend = -Inf), color = "darkgray",size=size) +
    #geom_segment(aes(y = -6, yend = 6, x = -Inf, xend = -Inf), color = "darkgray",size=size)+
    theme(
      axis.text.x =  element_blank(),
      axis.ticks.x = element_line(color = "darkgray"), 
      #axis.title.x = element_text(size = 15, color = "black", face = "bold"),
      axis.text.y = element_blank(),
      #axis.ticks.y = element_line(color = "darkgray"), 
      axis.title.x = element_text(size = 15, color = "black", face = "bold"),
      axis.title.y = element_text(size = 15, color = "black", face = "bold"),
      plot.margin = unit(c(0, 0, 0, 0), "cm")
      #axis.ticks.length = unit(0.25, "cm")
    )
  
  return(p)
}

widths=c(5,4)
pa<-gap(da,'Overall')

pb1<-gap(db1,'Age<40')
pb2<-gap2(db2,'Age>=40')
pb<-grid.arrange(pb1,pb2,nrow = 1,widths=widths)

pc1<-gap(dc1,'Multipe')
pc2<-gap2(dc2,'Single')
pc<-grid.arrange(pc1,pc2,nrow = 1,widths=widths)

pd1<-gap(dd1,'Male')
pd2<-gap2(dd2,'Female')
pd<-grid.arrange(pd1,pd2,nrow = 1,widths=widths)

pe1<-gap(de1,'Urban')
pe2<-gap2(de2,'Rural')
pe<-grid.arrange(pe1,pe2,nrow = 1,widths=widths)

pa <- pa + 
  labs(
    title = paste0("<span style='color:",custom_colors[1],";'>Consultation-only</span> vs. <span style='color:",custom_colors[2],";'>E-learning plus</span>")
  ) + 
  theme(
    plot.title = element_markdown(
      face = "bold",
      size = 20,
      hjust = 0.5
    )
  )
label_size =30
empty_plot <- ggplot() + 
  theme_void()
#p<-plot_grid(pc,pe,pd,pb,nrow=1)
p1<-plot_grid(empty_plot,pa,empty_plot,pb,empty_plot,nrow=1,rel_widths = c(1,3,1,3,1))
p2<-plot_grid(pe,pd,pc,nrow=1)
p<-plot_grid(p1,p2,nrow = 2)
# 空白主题
width=6

ggsave(filename = "github/fig_4b.pdf", plot = p, width = 12, height =12)