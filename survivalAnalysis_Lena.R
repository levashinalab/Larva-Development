##This script calculates the Kaplan-Meier curves for survival data 
## Created by Paola Carrillo-Bustamante 
## 24-10-2018

#remove old values to avoid trouble
rm(list=ls())

#load the necessary libraries and files. You might need to install the packages first
require(plyr) #necessary for data processing
require(gdata) #necessary to read xls files
library(survival)
source("https://bioconductor.org/biocLite.R")

library(ggplot2)
library(survminer)


TYPE<- "survival"

IN_DIR<-paste('/Volumes/abt.levashina/Project Development_AW_PCB_PS/rawData/', TYPE, '/', sep = "")

OUT_FILE<-paste("/Volumes/abt.levashina/Project Development_AW_PCB_PS/analysis/", Sys.Date(),"_",TYPE,"_survival.csv",sep = "")


#set the current directory to IN_DIR
setwd(IN_DIR)

#save all files wihtin the directory
nm <- list.files(path=IN_DIR)


#concatenate all data frames in the directory into one-------
df.orig<-do.call(rbind, lapply(nm, function(x) read.xls(x,sheet = 3)))  #sheet 3 is where Paula saved the data in the required format




### read data ----
survivalData<-read.csv("~/Documents/Projects/Malaria/rawData/Survival_all_mosquitoes.csv", sep = ';', header = TRUE)

##extract relevant information ----
## 1. comparison Infection vs Normal_BM
##survival fit 
fit1<-survfit(Surv(Time, Status) ~ Condition, data=survivalData[survivalData$Treatment != "anti-miR-276" &  survivalData$Feeding !="Water" & survivalData$Condition!="No_BM",])
#fit2<-survfit(Surv(Time, Status) ~ Condition + Treatment, data=survivalData[survivalData$Treatment != "anti-miR-276" &  survivalData$Feeding !="Water" & survivalData$Condition!="Infection",])


### 