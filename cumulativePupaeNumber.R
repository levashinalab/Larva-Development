##This script calculates the cumulative number of pupae per experiment
## Created by Paola Carrillo-Bustamante 
## 28-06-2018

#remove old values to avoid trouble
rm(list=ls())
#load the necessary libraries and files
suppressPackageStartupMessages(require(plyr, quietly = TRUE))


IN_FILE<-'~/Documents/Projects/LarvaeDevelopment/raw_Data/2018_06_28_development-data-28.csv'
OUT_FILE<-'~/Documents/Projects/LarvaeDevelopment/analysis/2018_06_28_cumulative_pupae.csv'
OUT_FILE_M<-'~/Documents/Projects/LarvaeDevelopment/analysis/2018_06_28_cumulative_pupae_mean.csv'

df.orig<-read.csv(IN_FILE, sep = ";", header = TRUE, row.names = NULL)

##calculate the cumulative number of pupaeting larvae per day (and the frequency) ----
df<-ddply(df.orig, .(Density, Strain, Ex.Repeat, Pan), function(X){
  cum.pupae.total<-cumsum(X$Pupae)
  cum.pupae.freq<-cum.pupae.total/X$Density
  Day <- X$Day
  data.frame(Day,cum.pupae.total, cum.pupae.freq)
})


#Save the median, mean, SD, and SE of the cum.pupae (needed for data fitting)
df.median<-ddply(df, .(Density, Strain, Ex.Repeat, Day), function(X)
  {
    median.total<-median(X$cum.pupae.total)
    median.freq<-median(X$cum.pupae.freq)
    mean.total<-mean(X$cum.pupae.total)
    mean.freq<-mean(X$cum.pupae.freq)
    data.frame(median.total, median.freq, mean.total, mean.freq)
  })


##save the transformed data---
write.csv(df, file = OUT_FILE, row.names = FALSE)
write.csv(df.median, file = OUT_FILE_M, row.names = FALSE)