##################################################################
###     This is a model of larval development ####
#####  simple larva dynamics to fit to Marta's data #####
##################################################################


#remove old values to avoid trouble
rm(list=ls())

#source grind and the models
#source("~/Nextcloud/Documents/Resources/Modelling/grindR/grindR.R")
source("~/Documents/Projects/LarvaeDevelopment/src/Larva-Development/larvaDevelopmentModels.R")


# Running the model
params<-c(gamma = 5, h = 100, delta = 0.1, tau = 10)
s_init<-c(L = 100, P=0)

solveLarva<-function(pars=params, times=seq(0,20,by=0.5), model=larva_growth, s = s_init)
{
  return(ode(y = s, times = times, func = model, parms = pars))    
}


out<-solveLarva(pars = params, model=larva_growth)
matplot(out[,1], out[,-1], lty = 1:3, lwd = c(2,2,1), type = "l", ylab = "Density", xlab = "Time (days)")

## Global Sensitivity
parRanges <- data.frame(min = c(0.1, 0.01, 50), max = c(3, 0.6, 1000))
rownames(parRanges) <- c("gamma", "delta", "h")

print(system.time(sR <- sensRange(func = solveLarva, parms = params, dist = "grid", sensvar = c("L", "P"), parRange = parRanges[3,], num = 100)))


summ.sR <- summary(sR)
par(mfrow=c(2, 2))
plot(summ.sR, xlab = "Time (days)", ylab = "Density",legpos = "topright", mfrow = NULL)
plot(summ.sR, xlab = "Time (days)", ylab = "Density", mfrow = NULL, quant = TRUE, col = c("lightblue", "darkblue"), legpos = "topright")
mtext(outer = TRUE, line = -1.5, side = 3, "Sensitivity to h", cex = 1.25)
par(mfrow = c(1, 1))





#set input file
IN_FILE<-"~/Documents/Projects/LarvaeDevelopment/analysis/2018_06_28_cumulative_pupae_mean.csv"

#read the data
df<-read.csv(IN_FILE, sep = ",", header = TRUE, row.names = NULL)

#test one data set
rsgG.100<-subset(df, Strain == "RsgG" & Density ==100)
#test one repeat
ex.4<-subset(rsgG.100, Ex.Repeat == 4, select = c("Day" , "median.total"))
names(ex.4)<-c("time", "P")

