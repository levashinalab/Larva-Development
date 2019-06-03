# This script plots the time courses of both, the proportion of pupaeting individuals as well as the cumulative number of pupae
# Created by Paola Carrillo-Bustamante 
# 05-07-2018

#remove old values to avoid trouble
rm(list=ls())

#load the necessary libraries and files
suppressPackageStartupMessages(require(ggplot2, quietly = TRUE))
source('~/Documents/Projects/Malaria/modeling/ABM/src/RScripts/ggplotThemes.R') #update the path to where you saved this file
require(gdata) #necessary to read xls files
require(plyr)

####TO DO:::___________
# if sum(X$Pan == 0) then remove than Pan


# 0. set the parameters -----
TYPE<- 'development_low'

# set the names of the files we are reading from:
IN_DIR<-paste('/Volumes/abt.levashina/Project Development_AW_PCB_PS/rawData/', TYPE, '/', sep = "")

IN_FILE_CUM<-paste("/Volumes/abt.levashina/Project Development_AW_PCB_PS/analysis/", TYPE,"/", Sys.Date(),"_cumulative_pupae_mean.csv",sep = "")

#set the names of the files we are saving
OUT_FILE<-paste("/Volumes/abt.levashina/Project Development_AW_PCB_PS/figures/", TYPE, "/",Sys.Date(),"_development.pdf",sep = "")

OUT_CUM<-paste("/Volumes/abt.levashina/Project Development_AW_PCB_PS/figures/",TYPE,"/", Sys.Date(),"_cumulativePupae.pdf",sep = "")

#set the current directory to IN_DIR
setwd(IN_DIR)

#save all files wihtin the directory
nm <- list.files(path=IN_DIR)

#concatenate all data frames in the directory into one-------
if(TYPE == "food"| TYPE == "volume"| TYPE == "development_low")
{
  df.orig<-read.xls(nm)
  #exclude one repeat that didn't have anything
  if(TYPE == "food")
  {
    df.orig<-df.orig[!(df.orig$Density == 100 & df.orig$Pan ==1 & df.orig$Ex.Repeat ==3),]
  }
}else{
  df.orig<-do.call(rbind, lapply(nm, function(x) read.xls(x,sheet = 3)))  #sheet 3 is where Paula saved the data in the required format
}

#calculate the median from all the pans 
df.formatted<-ddply(df.orig, .(Day, Density, Ex.Repeat, Strain, Temperature), function(X){
  Pupae<-median(X$Pupae)
  data.frame(Pupae)
})

### plot the time course data of the fraction of pupaeting individuals
#this plot includes the invidiual data points of every ex.repeat
pl1<-ggplot(df.formatted, aes(x = Day, y = Pupae/Density,colour = as.factor(Density))) + geom_point()+facet_grid(Temperature~Strain)  +ylab("Proportion of pupaeting larvae") +basic_theme+scale_color_manual(values=c("black","red", "blue","orange")) +stat_summary(geom="line", fun.y = "mean") + stat_summary(fun.data = mean_se) 

#this plot just plots the summary, i.e., mean with se
pl1.pulled<-ggplot(df.formatted, aes(x = Day, y = Pupae/Density, colour = as.factor(Density)))+facet_grid(Temperature~Strain)  + stat_summary(geom="point", fun.y = mean)+stat_summary(geom="line", fun.y = "mean") + stat_summary(fun.data = mean_se)+ylab("Proportion of pupaeting larvae")+scale_color_manual(values=c("black","red", "blue", "orange")) +basic_theme


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
pl2<-ggplot(subset(df,Strain == "S1"), aes(x = Day, y = median.freq*100, colour = as.factor(Density)))+ facet_grid(Temperature~Strain)+ stat_summary(geom="line", fun.y="mean") + geom_point(stat = "summary", fun.y = mean)+ stat_summary(fun.data = mean_se)+basic_theme +ylab("Developed larvae (%)")+scale_color_manual(values=c("black","red", "blue", "orange"))

pl2_t<-ggplot(subset(df,Strain == "S1"), aes(x = Day, y = median.freq*100, colour = as.factor(Temperature)))+ facet_grid(Density~Strain)+ stat_summary(geom="line", fun.y="mean") + geom_point(stat = "summary", fun.y = mean)+ stat_summary(fun.data = mean_se)+basic_theme +ylab("Developed larvae (%)")+scale_color_manual(values=c("orange","darkgreen", "darkblue", "black"))


#this plot just plots the summary, i.e., mean with se
pl2.pulled<-ggplot(subset(df,Strain == "S1"), aes(x = Day, y = median.total, colour = as.factor(Density)))+ facet_grid(Temperature~Strain)+ stat_summary(geom="line", fun.y="mean") + stat_summary(geom="point", fun.y = mean)+stat_summary(fun.data = mean_se)+basic_theme  +ylab("Developed larvae") +scale_color_manual(values=c("black","red", "blue", "orange" ))

pl2.pulled_t<-ggplot(subset(df,Strain == "S1"), aes(x = Day, y = median.total, colour = as.factor(Temperature)))+ facet_grid(Density~Strain)+ stat_summary(geom="line", fun.y="mean") + stat_summary(geom="point", fun.y = mean)+stat_summary(fun.data = mean_se)+basic_theme  +ylab("Number of developed larvae") +scale_color_manual(values=c("orange","darkgreen", "darkblue","black"))

#save the plot
pdf(OUT_CUM, width = 9, height = 9)
pl2
pl2_t
pl2.pulled
pl2.pulled_t
dev.off()


### plots for the presentation ------
df$Temperature<-as.factor(df$Temperature)
df$Temperature<-factor(df$Temperature,levels = c(28,26,24))

pdf('~/Documents/Presentations/LunchSeminarMPI/resultsI.pdf', width = 6, height = 9)
ggplot(subset(df,Strain == "S1"& Density == 100), aes(x = Day, y = median.freq*100, colour = as.factor(Density)))+ facet_wrap(~as.factor(Temperature),ncol = 1)+ stat_summary(geom="line", fun.y="mean") + geom_point(stat = "summary", fun.y = mean)+ stat_summary(fun.data = mean_se)+basic_theme +ylab("Developed larvae (%)")+scale_color_manual(values=c("black","red", "blue","orange")) +ylim(c(0,100))
dev.off()


pdf('~/Documents/Presentations/LunchSeminarMPI/resultsII.pdf', width = 6, height = 6)
ggplot(subset(df,Strain == "S1"), aes(x = Day, y = median.freq*100, colour = as.factor(Density)))+ facet_wrap(~as.factor(Temperature),ncol = 1)+ stat_summary(geom="line", fun.y="mean") + geom_point(stat = "summary", fun.y = mean)+ stat_summary(fun.data = mean_se)+basic_theme +ylab("Developed larvae (%)")+scale_color_manual(values=c("black","red", "blue", "orange")) +ylim(c(0,100))
dev.off()
