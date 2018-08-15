##This script calculates the cumulative number of pupae per experiment
## Created by Paola Carrillo-Bustamante 
## 28-06-2018

#remove old values to avoid trouble
rm(list=ls())
#load the necessary libraries and files. You might need to install the packages first
require(plyr) #necessary for data processing
require(gdata) #necessary to read xls files

IN_DIR<-'/Volumes/abt.levashina/Project Development_AW_PCB_PS/rawData/development/'

OUT_FILE<-paste("/Volumes/abt.levashina/Project Development_AW_PCB_PS/analysis/", Sys.Date(),"_development_cumulative_pupae.csv",sep = "")
  
OUT_FILE_M<-paste("/Volumes/abt.levashina/Project Development_AW_PCB_PS/analysis/", Sys.Date(),"_development_cumulative_pupae_mean.csv",sep = "")

#set the current directory to IN_DIR
setwd(IN_DIR)

#save all files wihtin the directory
nm <- list.files(path=IN_DIR)

#concatenate all data frames in the directory into one-------
df.orig<-do.call(rbind, lapply(nm, function(x) read.xls(x,sheet = 3)))  #sheet 3 is where Paula saved the data in the required format

##calculate the cumulative number of pupaeting larvae per day (and the frequency) ----
df<-ddply(df.orig, .(Density, Strain, Ex.Repeat, Pan, Temperature), function(X){
  cum.pupae.total<-cumsum(X$Pupae)
  cum.pupae.freq<-cum.pupae.total/X$Density
  Day <- X$Day
  data.frame(Day,cum.pupae.total, cum.pupae.freq)
})

#Save the median, mean, SD, and SE of the cum.pupae (needed for data fitting)
df.median<-ddply(df, .(Density, Strain, Ex.Repeat, Day, Temperature), function(X)
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