#  Process and refromat the data to fit dMOd requirements
#  Paola Carrillo-Bustamante
#  Max Planck Institute for Infection Biology
#  13-07-2018


#remove old values to avoid trouble
rm(list=ls())

#load all necessary libraries
library(plyr)

STRAIN = "S1"

IN_FILE<-"/Volumes/abt.levashina/Project Development_AW_PCB_PS/analysis/development/2019-02-12_cumulative_pupae_mean.csv"
OUT_FILE_TRAINING<-paste("/Volumes/abt.levashina/Project Development_AW_PCB_PS/analysis/development/modelling/2019-02-12_cumulative_pupae_mean_",STRAIN,"_dMod_training.csv", sep = "")
OUT_FILE_TEST<-paste("/Volumes/abt.levashina/Project Development_AW_PCB_PS/analysis/development/modelling/2019-02-12_cumulative_pupae_mean_",STRAIN,"_dMod_test.csv", sep = "")

#read file
df<-read.csv(IN_FILE, sep = ",", header = TRUE, row.names = NULL)

#select the strain of interest
df.short<-subset(df, Strain == STRAIN)

df.test<-subset(df.short, Temperature == 28)
#exclude the last 2 repeats (to be used for validation)
n<-(length(unique(df.test$Ex.Repeat)))
df.training<-subset(df.short, Ex.Repeat != n-1 & Ex.Repeat !=n)
df.test<-subset(df.short, Ex.Repeat == n-1 | Ex.Repeat == n)


#calculate mean and standard error out of every repeat
tmp.df<-ddply(df.training, .(Strain, Density, Day, Temperature), function(X)
{
  P_mean<-mean(X$median.total)
  P_se<-sd(X$median.total)/(sqrt(nrow(X)))
  data.frame(P_mean, P_se)
})

#remove missing values
tmp.df<-na.omit(tmp.df)

#Define data to be fitted by the model and put it into the required format
time<-subset(tmp.df, select = "Day")
name<-rep("Pupae", nrow(tmp.df))
value<-subset(tmp.df,  select = "P_mean")
sigma<-subset(tmp.df, select = "P_se")
condition<-subset(tmp.df, select = "Density")
condition2<-subset(tmp.df, select = "Temperature")

#rename condition to match the above definition
condition[condition=="20",]<-"20Density"
condition[condition=="100",]<-"100Density"
condition[condition=="250",]<-"250Density"
condition[condition=="500",]<-"500Density"
condition[condition=="1000",]<-"1000Density"

condition2[condition2=="24",]<-"24Temp"
condition2[condition2=="26",]<-"26Temp"
condition2[condition2=="28",]<-"28Temp"

df.Pupae<-cbind(time, name, value, sigma, condition, condition2)
names(df.Pupae)<-c("time", "name", "value", "sigma", "condition", "condition2")

#calculate mean and standard error out of every repeat
tmp.df<-ddply(df.test, .(Strain, Density, Day, Temperature), function(X)
{
  P_mean<-mean(X$median.total)
  P_se<-sd(X$median.total)/(sqrt(nrow(X)))
  data.frame(P_mean, P_se)
})

#remove missing values
tmp.df<-na.omit(tmp.df)

#Define data to be fitted by the model and put it into the required format
time<-subset(tmp.df, select = "Day")
name<-rep("Pupae", nrow(tmp.df))
value<-subset(tmp.df,  select = "P_mean")
sigma<-subset(tmp.df, select = "P_se")
condition<-subset(tmp.df, select = "Density")
condition2<-subset(tmp.df, select = "Temperature")

#rename condition to match the above definition
condition[condition=="20",]<-"20Density"
condition[condition=="100",]<-"100Density"
condition[condition=="250",]<-"250Density"
condition[condition=="500",]<-"500Density"
condition[condition=="1000",]<-"1000Density"

condition2[condition2=="24",]<-"24Temp"
condition2[condition2=="26",]<-"26Temp"
condition2[condition2=="28",]<-"28Temp"

df.Test<-cbind(time, name, value, sigma, condition, condition2)
names(df.Test)<-c("time", "name", "value", "sigma", "condition", "condition2")


##save the data
write.csv(df.Pupae, file = OUT_FILE_TRAINING, row.names = FALSE)
write.csv(df.Test, file = OUT_FILE_TEST, row.names = FALSE)