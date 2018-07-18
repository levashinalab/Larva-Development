#  Process and refromat the data to fit dMOd requirements
#  Paola Carrillo-Bustamante
#  Max Planck Institute for Infection Biology
#  13-07-2018


#remove old values to avoid trouble
rm(list=ls())

#load all necessary libraries
library(plyr)

IN_FILE<-"~/Documents/Projects/LarvaeDevelopment/analysis/2018_07_18_cumulative_pupae_mean.csv"
OUT_FILE<-"~/Documents/Projects/LarvaeDevelopment/analysis/2018_07_18_cumulative_pupae_dMod_RsgG.csv"

#read file
df<-read.csv(IN_FILE, sep = ",", header = TRUE, row.names = NULL)

#calculate mean and standard error out of every repeat
tmp.df<-ddply(df, .(Strain, Density, Day), function(X)
{
  P_mean<-mean(X$median.total)
  P_se<-sd(X$median.total)/(sqrt(nrow(X)))
  data.frame(P_mean, P_se)
})

#remove missing values
tmp.df<-na.omit(tmp.df)

#Define data to be fitted by the model (RsgG) and put it into the required format
time<-subset(tmp.df, Strain =="RsgG", select = "Day")
name<-rep("Pupae", nrow(subset(tmp.df, Strain =="RsgG")))
value<-subset(tmp.df, Strain =="RsgG", select = "P_mean")
sigma<-subset(tmp.df, Strain =="RsgG", select = "P_se")
condition<-subset(tmp.df, Strain =="RsgG", select = "Density")

#rename condition to match the above definition
condition[condition=="100",]<-"100Density"
condition[condition=="250",]<-"250Density"
condition[condition=="500",]<-"500Density"

df.Pupae<-cbind(time, name, value, sigma, condition)
names(df.Pupae)<-c("time", "name", "value", "sigma", "condition")


##save the data

write.csv(df.Pupae, file = OUT_FILE, row.names = FALSE)