# Script to plot Figure 2

# Author: Etienne Henry
# Date: 10/2022

# Load packages ----------------------------------------------------------------

library(dplyr)
library(tidyr)
library(plyr)
library(ggplot2)
library(RColorBrewer)

# Function color

f <- function(pal) brewer.pal(brewer.pal.info[pal, "maxcolors"], pal)

# Binary validation CLM -------------------------------------------------------


df.acc.binary <- read.table("dataset/birds/processed_data/accuracy_validation_binary_TH_nTH_noLCmaybe_nomarine.csv")


df.acc.binary$criteria <- factor(df.acc.binary$criteria,
                                 levels=c("RL_cat","CS","A2","A3","A4","B1","B2","C1","C2","D1","D2"))

col <- c("#d0cece","#9e1741","#f46b43","#ffaf5f","#fee08b","#67c1a4","#abdda5","#d18dc6","#ffcef9","#92b7d9","#d9e4f4")


# Sensitivity

mean_sens <- mean(df.acc.binary$sensitivity[2:10])

ggplot(df.acc.binary,aes(x=criteria,y=sensitivity,col=criteria))+
  geom_hline(yintercept=mean_sens, linetype="dashed")+
  geom_point(size = 5)+
  scale_color_manual(values=col)+ 
  ylim(0.4,1)+
  ylab("Sensitivity")+
  theme_bw() + theme(legend.position = "none")+
  theme(       legend.title = element_text(size=12, face="bold"),
               legend.key.height = grid::unit(1,"cm"),          
               legend.key.width  = grid::unit(0.6,"cm"), 
               axis.title.x= element_blank(),
               axis.text.x=element_text(size=12),
               axis.ticks.x=element_line(size=0.4),
               axis.text.y = element_text(size=12),           
               axis.ticks = element_line(size=0.4),               
               axis.title = element_text(size=12, face="bold"))


# Specificity 

mean_spe <- mean(df.acc.binary$specificity[2:10])

ggplot(df.acc.binary,aes(x=criteria,y=specificity,col=criteria))+
  geom_hline(yintercept=mean_spe, linetype="dashed")+
  geom_point(size = 5)+
  ylim(0.4,1)+
  scale_color_manual(values=col)+ 
  ylab("Specificity")+
  theme_bw() + theme(legend.position = "none")+
  theme(       legend.title = element_text(size=12, face="bold"),
               legend.key.height = grid::unit(1,"cm"),          
               legend.key.width  = grid::unit(0.6,"cm"), 
               axis.title.x= element_blank(),
               axis.text.x=element_text(size=12),
               axis.ticks.x=element_line(size=0.4),
               axis.text.y = element_text(size=12),            
               axis.ticks = element_line(size=0.4),               
               axis.title = element_text(size=12, face="bold"))

# TSS
mean_tss <- mean(df.acc.binary$TSS[2:10])
ggplot(df.acc.binary,aes(x=criteria,y=TSS,col=criteria))+
  geom_hline(yintercept=mean_tss, linetype="dashed")+
  geom_point(size = 5)+
  scale_color_manual(values=col)+ 
  ylim(0,1)+
  ylab("TSS")+

  theme_bw() + theme(legend.position = "none")+
  theme(       legend.title = element_text(size=12, face="bold"),
               legend.key.height = grid::unit(1,"cm"),          
               legend.key.width  = grid::unit(0.6,"cm"), 
               axis.title.x= element_blank(),
               axis.text.x=element_text(size=12),
               axis.ticks.x=element_line(size=0.4),
               axis.text.y = element_text(size=12),           
               axis.ticks = element_line(size=0.4),               
               axis.title = element_text(size=12, face="bold"))

# Average error (not used in the analysis)

ggplot(df.acc.binary,aes(x=criteria,y=average_error,col=criteria))+
  geom_point(size = 10)+
  scale_color_manual(values=col)+ 
  ylim(1,0)+
  ylab("Average Error")+
  theme_bw() + theme(legend.position = "none")+
  theme(       legend.title = element_text(size=12, face="bold"),
               legend.key.height = grid::unit(1,"cm"),           
               legend.key.width  = grid::unit(0.6,"cm"), 
               axis.title.x= element_blank(),
               axis.text.x=element_text(size=12),
               axis.ticks.x=element_line(size=0.4),
               axis.text.y = element_text(size=12),            
               axis.ticks = element_line(size=0.4),               
               axis.title = element_text(size=12, face="bold"))

# END -----
