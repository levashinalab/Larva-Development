# This script plots the time courses of both, the proportion of pupaeting individuals as well as the cumulative number of pupae
# Created by Paola Carrillo-Bustamante 
# 05-07-2018

#remove old values to avoid trouble
rm(list=ls())

#load the necessary libraries and files
suppressPackageStartupMessages(require(ggplot2, quietly = TRUE))
source('~/Documents/Projects/Malaria/modeling/ABM/src/RScripts/ggplotThemes.R') #update the path to where you saved this file
require(gdata) #necessary to read xls files


# 0. set the parameters -----
TYPE<- 'development'

# set the names of the files we are reading from:
IN_DIR<-paste('/Volumes/abt.levashina/Project Development_AW_PCB_PS/rawData/', TYPE, '/', sep = "")

IN_FILE_CUM<-paste("/Volumes/abt.levashina/Project Development_AW_PCB_PS/analysis/", Sys.Date(),"_",TYPE,"_cumulative_pupae_mean.csv",sep = "")

#set the names of the files we are saving
OUT_FILE<-paste("/Volumes/abt.levashina/Project Development_AW_PCB_PS/figures/", Sys.Date(),"_",TYPE,"_development.pdf",sep = "")

OUT_CUM<-paste("/Volumes/abt.levashina/Project Development_AW_PCB_PS/figures/", Sys.Date(),"_",TYPE,"_cumulativePupae.pdf",sep = "")

#set the current directory to IN_DIR
setwd(IN_DIR)

#save all files wihtin the directory
nm <- list.files(path=IN_DIR)

#concatenate all data frames in the directory into one-------
df.orig<-do.call(rbind, lapply(nm, function(x) read.xls(x,sheet = 3)))  #sheet 3 is where Paula saved the data in the required format


#calculate the median from all the pans 
df.formatted<-ddply(df.orig, .(Day, Density, Ex.Repeat, Strain, Temperature), function(X){
  Pupae<-median(X$Pupae)
  data.frame(Pupae)
})

### plot the time course data of the fraction of pupaeting individuals
#this plot includes the invidiual data points of every ex.repeat
pl1<-ggplot(df.formatted, aes(x = Day, y = Pupae/Density,colour = as.factor(Density))) + geom_point()+facet_grid(Temperature~Strain)  +ylab("Proportion of pupaeting larvae") +basic_theme+scale_color_manual(values=c("black","red", "blue")) +stat_summary(geom="line", fun.y = "mean") + stat_summary(fun.data = mean_se) 

#this plot just plots the summary, i.e., mean with se
pl1.pulled<-ggplot(df.formatted, aes(x = Day, y = Pupae/Density, colour = as.factor(Density)))+facet_grid(Temperature~Strain)  + stat_summary(geom="point", fun.y = mean)+stat_summary(geom="line", fun.y = "mean") + stat_summary(fun.data = mean_se)+ylab("Proportion of pupaeting larvae")+scale_color_manual(values=c("black","red", "blue")) +basic_theme


#save the plot
pdf(OUT_FILE, width = 9, height = 9)
pl1
pl1.pulled
dev.off()


## 2. continue with the cumulative pupae data ----
#read the data
df<-read.csv(IN_FILE_CUM, sep = ",", header = TRUE, row.names = NULL)


##plot the time course data of the cumulative sum of pupaeting individuals
#this plot includes dthe invidiual data points of every ex.repeat
pl2<-ggplot(df, aes(x = Day, y = median.freq, colour = as.factor(Density)))+ facet_grid(Temperature~Strain)+ stat_summary(geom="line", fun.y="mean") + geom_point(stat = "summary", fun.y = mean)+ stat_summary(fun.data = mean_se)+basic_theme +ylab("Cumulative frequency of pupae")+scale_color_manual(values=c("black","red", "blue"))

#this plot just plots the summary, i.e., mean with se
pl2.pulled<-ggplot(df, aes(x = Day, y = median.total, colour = as.factor(Density)))+ facet_grid(Temperature~Strain)+ stat_summary(geom="line", fun.y="mean") + stat_summary(geom="point", fun.y = mean)+stat_summary(fun.data = mean_se)+basic_theme  +ylab("Cumulative number of pupae") +scale_color_manual(values=c("black","red", "blue" ))


#save the plot
pdf(OUT_CUM, width = 9, height = 9)
pl2
pl2.pulled
dev.off()




