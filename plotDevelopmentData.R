##This script plots the time courses of both, the proportion of pupaeting individuals as well as the cumulative number of pupae
## Created by Paola Carrillo-Bustamante 
## 05-07-2018

#remove old values to avoid trouble
rm(list=ls())
#load the necessary libraries and files
suppressPackageStartupMessages(require(ggplot2, quietly = TRUE))
source('~/Documents/Projects/Malaria/modeling/ABM/src/RScripts/ggplotThemes.R')



# 0. set the parameters -----
# set the names of the files we are reading from:
IN_FILE<-'~/Documents/Projects/LarvaeDevelopment/raw_Data/2018_06_28_development-data-28.csv'
IN_FILE_CUM<-'~/Documents/Projects/LarvaeDevelopment/analysis/2018_06_28_cumulative_pupae.csv'

#set the names of the files we are saving
OUT_FILE<-'~/Documents/Projects/LarvaeDevelopment/figures/2018_06_28_summary_development.pdf'
OUT_CUM<-'~/Documents/Projects/LarvaeDevelopment/figures/2018_06_28_sumulativePupae.pdf'


## 1 . start with the time course data ----
#read the data
df.orig<-read.csv(IN_FILE, sep = ";", header = TRUE, row.names = NULL)



### plot the time course data of the fraction of pupaeting individuals
pl1<-ggplot(df.orig, aes(x = Day, y = Pupae/Density)) + geom_point(aes(colour = as.factor(Ex.Repeat))) +facet_grid(Density~Strain) +stat_summary(geom="line", fun.y = "mean") + stat_summary(fun.data = mean_se, geom = "errorbar")+ylab("Proportion of pupaeting larvae") +basic_theme +ylim(c(0,0.8))

#save the plot
pdf(OUT_FILE, width = 9, height = 9)
pl1
dev.off()


## 2. continue with the cumulative pupae data ----
#read the data
df<-read.csv(IN_FILE_CUM, sep = ",", header = TRUE, row.names = NULL)


##plot the time course data of the cumulative sum of pupaeting individuals
pl2<-ggplot(df, aes(x = Day, y = cum.pupae.freq))+ geom_point(aes(colour = as.factor(Ex.Repeat)))+ stat_summary(geom="line", fun.y="mean") + geom_point(stat = "summary", fun.y = mean)+ stat_summary(fun.data = mean_se, geom = "errorbar")+ facet_grid(Density~Strain)+basic_theme +ylim(c(0,0.8)) +ylab("Cumulative frequency of pupae")

#save the plot
pdf(OUT_CUM, width = 9, height = 9)
pl2
dev.off()




