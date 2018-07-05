##################################################################
### This is a model of larval development ####
#####  simple larva dynamics to fit to Marata's data #####
##################################################################


#remove old values to avoid trouble
#rm(list=ls())

#source grind and the models
source("~/Nextcloud/Documents/Resources/Modelling/grindR/grindR.R")
source("~/Documents/Projects/LarvaeDevelopment/src/larvaDevelopmentModels.R")


##### 1. define parameters  and run the model-----
params<-c(gamma = 5, h = 100, delta = 0.1, tau = 10)
s_init<-c(L = 100, P=0)


run(odes = larva_growth, parms = params, state = s_init, tmax = 20, tstep = 0.1)

params<-c(gamma = 1, delta = 0.1, tau = 10, h = 500, h2 = 50)
run(odes=larva_growth_delay,parms = params, state = s_init, tmax = 20, tstep = 0.1)
