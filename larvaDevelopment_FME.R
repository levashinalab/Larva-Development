#  ODE model of larval development #
#  Sensitivity Analysis and Data fitting
#  Paola Carrillo-Bustamante
#  Max Planck Institute for Infection Biology
#  13-07-2018


#remove old values to avoid trouble
rm(list=ls())

#load all necessary libraries
library(deSolve)      
library(rootSolve)   
library(FME)         

#source the models
source("~/Documents/Projects/LarvaeDevelopment/src/Larva-Development/larvaDevelopmentModels.R")

## Define the model run ----
params<-c(gamma = 0.3, h = 150, delta_L = 1)  # parameters
s_init<-c(L = 100, P=0)                                 # initial states

solveLarva<-function(pars=params, times=seq(0,20,by=0.5), model=larva_growth, s = s_init)
{
  return(ode(y = s, times = times, func = model, parms = pars))    
}


#run the model
out<-solveLarva(pars = params, model=larva_growth)     #model solution
matplot(out[,1], out[,-1], lty = 1:3, lwd = c(2,2,1), type = "l", ylab = "Density", xlab = "Time (days)")  #plot the model


##The Data: read and load the data of RsgG----
IN_FILE<-"~/Documents/Projects/LarvaeDevelopment/analysis/2018_06_28_cumulative_pupae_mean.csv"
df<-read.csv(IN_FILE, sep = ",", header = TRUE, row.names = NULL)
data.Pupae<-subset(df, Strain =="RsgG" & Density ==500, select = 4:5)
names(data.Pupae)[1]<-"time"
names(data.Pupae)[2]<-"P"
#data.Pupae$P<-data.Pupae$P/100

#define function that determines the "cost"
LarvaCost<-function(pars)
{
  out<-(solveLarva(pars))
  cost<-modCost(model = out, obs = data.Pupae[16:30,], scaleVar = TRUE)
  return(cost)
}

plot(LarvaCost(params), xlab = "time")

## local sensitivity
Sfun<-sensFun(LarvaCost,params)
summary(Sfun)
plot(Sfun, which = "P", xlab = "time", lwd = 2)

#collinearity
ident<-collin(Sfun)
print(ident)

plot(P~time, data = data.Pupae[16:30,])
lines(P~time, data = out, col = "red")

##fit the model to the data
Fit<-modFit(f = LarvaCost, p = log(params))

ini<-solveLarva(pars = params)
final<-solveLarva(pars = coef(Fit))

plot(data.Pupae[16:30,])
lines(P~time, data = ini, lty = 2)
lines(P~time, data = final, col = "red")

