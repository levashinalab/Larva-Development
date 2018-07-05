##This script calculates the cumulative number of pupae per experiment
## Created by Paola Carrillo-Bustamante 
## 28-06-2018

#remove old values to avoid trouble
rm(list=ls())
#load the necessary libraries and files

suppressPackageStartupMessages(require(ggplot2, quietly = TRUE))
suppressPackageStartupMessages(require(plyr, quietly = TRUE))
source('~/Documents/Projects/Malaria/modeling/ABM/src/RScripts/ggplotThemes.R')


IN_FILE<-'~/Documents/Projects/LarvaeDevelopment/raw_Data/2018_06_28_development-data-28.csv'
OUT_FILE<-'~/Documents/Projects/LarvaeDevelopment/analysis/2018_06_28_cumulative_pupae.csv'


df.orig<-read.csv(IN_FILE, sep = ";", header = TRUE, row.names = NULL)

##calculate the cumulative number of pupaeting larvae per day (and the frequency) ----
df<-ddply(df.orig, .(Density, Strain, Ex.Repeat, Pan), function(X){
  cum.pupae.total<-cumsum(X$Pupae)
  cum.pupae.freq<-cum.pupae.total/X$Density
  Day <- X$Day
  data.frame(Day,cum.pupae.total, cum.pupae.freq)
})


## I don't need this any longer, since ggplot can immedaitely plot the statistics.
##  I am saving it here because it might be useful later
## df.median<-ddply(df, .(Density, Strain, Ex.Repeat, Day), function(X)
##  {
##    median.total<-median(X$cum.pupae.total)
##    median.freq<-median(X$cum.pupae.freq)
##    data.frame(median.total, median.freq)
##  })


##save the transformed data---
write.csv(df, file = OUT_FILE, row.names = FALSE)