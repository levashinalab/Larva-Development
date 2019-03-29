##This script generates hazard fuctions for infected mosquitoes using the hazard functions of healthy individuals
## Created by Paola Carrillo-Bustamante 
## 19-02-2019

#remove old values to avoid trouble
rm(list=ls())

#load the necessary libraries and files. You might need to install the packages first
require(plyr) #necessary for data processing
require(gdata) #necessary to read xls files
library(ggplot2)
source('~/Documents/Projects/Malaria/modeling/ABM/src/RScripts/ggplotThemes.R') #update the path to where you saved this file


TYPE<- "survival"

IN_FILE<-paste('/Volumes/abt.levashina/Project Development_AW_PCB_PS/analysis/', TYPE, '/',Sys.Date(),"_predicted_hazard.csv",sep = "")

OUT_DIR<-'~/Documents/Projects/Malaria/preparedData/deathRates/'

#save all files wihtin the directory
df <- read.csv2(IN_FILE)
df.short<-subset(df, Hazard <=1.1, select= c(1,2,5))
Condition<-rep('Control',nrow(df.short))
df.short<-cbind(df.short,Condition)

df.inf<-ddply(df.short, .(Density), function(X){
  Hazard<-X$Hazard*1.1
  Condition<-rep('Infected',length(Hazard))
  Time<-X$Time
  data.frame(Time,Hazard,Condition)
})

df.inf<-subset(df.inf,select=c(2,3,1,4))
df.total<-rbind(df.short, df.inf)

for (i in unique(df$Density))
{
  file <- paste(OUT_DIR,i,'_hazard_control.txt', sep = "")
  tmp<-subset(df.short, Density == i)$Hazard
  write.table(tmp, file, sep = "\t", row.names = FALSE, col.names = FALSE)
}

pdf(paste(OUT_DIR, 'predictedHazardWithInfection.pdf',sep=""))
ggplot(df.total, aes(x = Time, y = Hazard, colour = as.factor(Density),linetype=Condition)) +geom_line(size = 1.25) + paos_theme +scale_color_manual(values=c("black", "red", "blue"),name = 'Density')+ylim(c(0,1.19))
dev.off()


