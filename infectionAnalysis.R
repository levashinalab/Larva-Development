#remove old values to avoid trouble
rm(list=ls())
#load the necessary libraries and files. You might need to install the packages first
require(plyr) #necessary for data processing
require(gdata) #necessary to read xls files
library(ggplot2)
source('~/Documents/Projects/Malaria/modeling/ABM/src/RScripts/ggplotThemes.R')
TYPE<- "infection"

IN_DIR<-paste('/Volumes/abt.levashina/Project Development_AW_PCB_PS/rawData/', TYPE, '/', sep = "")

OUT_FILE<-paste("/Volumes/abt.levashina/Project Development_AW_PCB_PS/analysis/",TYPE,"/", Sys.Date(),"_infection_oocysts.csv",sep = "")

OUT_FILE_S<-paste("/Volumes/abt.levashina/Project Development_AW_PCB_PS/analysis/",TYPE,"/", Sys.Date(),"_infection_spz.csv",sep = "")

FILE<-paste(IN_DIR, "Infection.xlsx", sep = "")
df.ooc<-read.xls(FILE, sheet = 1)
df.spz<-read.xls(FILE, sheet = 2)


#to do: 
# 1. analyze oocysts: calculate the median of each repeat
summary_oocysts<-ddply(df.ooc, .(Infection, Density), function(X){
  prev<-1-nrow(subset(X, Oocyst==0))/nrow(X)
  df<-subset(X, Oocyst>0)
  ooc_median<-median(X$Oocyst)
  ooc_median_short<-median(df$Oocyst)
  data.frame(ooc_median,ooc_median_short, prev)
})


#calculate the mean out of every experiment for the sporozoites----
summary_sporos<-ddply(df.spz, .(Infection, Density), function(X){
  mean_sporo<-mean(X$Sporos)
  data.frame(mean_sporo)
})

##save the data frame---
write.csv(summary_oocysts, file = OUT_FILE, row.names = FALSE)

write.csv(summary_sporos, file = OUT_FILE_S, row.names = FALSE)



# 2. make the mean out of both persons for the sporozoites data
# 3. save the data into one DF


