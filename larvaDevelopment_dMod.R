#  ODE model of larval development #
#  Sensitivity Analysis and Data fitting
#  Paola Carrillo-Bustamante
#  Max Planck Institute for Infection Biology
#  13-07-2018


#remove old values to avoid trouble
rm(list=ls())

#load all necessary libraries
library(dMod)      
library(ggplot2)
library (tibble)
library(plyr)
source('~/Documents/Projects/LarvaeDevelopment/src/Larva-Development/larvaDevelopmentModels.R')
source('~/Documents/Projects/LarvaeDevelopment/src/Larva-Development/computeAICc.R')
source('~/Documents/Projects/Malaria/modeling/ABM/src/RScripts/ggplotThemes.R')


#set parameters
P_FILE<-"~/Documents/Projects/LarvaeDevelopment/analysis/2018_07_18_cumulative_pupae_dMod_RsgG.csv"

OUT_DIR<-paste("~/Documents/Projects/LarvaeDevelopment/data_fitting/", Sys.Date(), "/",sep = "")
OUT_FIG<-paste("~/Documents/Projects/LarvaeDevelopment/figures/fitting/", Sys.Date(), "/",sep = "")
STRAIN<-"RsgG_fixed_growth_TypeI_"


ifelse(!dir.exists(OUT_DIR), dir.create(OUT_DIR), FALSE)
ifelse(!dir.exists(OUT_FIG), dir.create(OUT_FIG), FALSE)


#FIXED<-TRUE

# Define the model in dMod style---------------
#define the ode Model (see the larvaDevelopmentModels.R for the exact definition of the models)
f<-f1
myevent <- eventlist(var = "gamma", time = "t_on", value = "gamma_on" , method = "replace") #necessary to make a piecewise model (parameter gamma is set as a variable that "rutns on" as an event)

#ODE model
model0<-odemodel(f, modelname = "larvaGrowth",compile = TRUE, events = myevent)

#Prediction function
x<-Xs(model0)

#Define observables and generate observation function g ------
observables<-eqnvec(
  Pupae = "P"
)

# Generate observation functions
g <- Y(observables, x, compile = TRUE, modelname = "obsfn", attach.input = FALSE)

# Make a prediction of the observables based on initial parameter values (just to see how it would look like)
#parTest<-c(L = 20, P = 0, gamma = 0, delta_L = 0.1, h = 100, gamma_on = 0.9, t_on = 6)
times <- seq(0, 18, len = 100)
#prediction <- (g*x)(times, parTest)
#plot(prediction)


# Define parameter transformation for the different experimental conditions -----------------
# Get all parameters
innerpars <- getParameters(g*x)

# Start with the identity transformation
trafo <- repar("x~x", x = innerpars)

# Set some initial value parameters
trafo <- repar("x~100", x = "L", trafo)
trafo <- repar("x~0", x = "P", trafo)
trafo <- repar("x~0", x = "gamma", trafo)

# Explicit log-transform of all parameters : assures that they will not take negative values
trafo <- repar("x~exp(x)", x = innerpars, trafo)

## Split transformation into three
trafo1 <- trafo2<- trafo3<-trafo

# Set the density conditions
trafo1["L"] <- "100"
trafo2["L"] <- "250"
trafo3["L"] <- "500"

# Generate parameter transformation functions
p <- NULL
p <- p + P(trafo1, condition = "100Density")
p <- p + P(trafo2, condition = "250Density")
p <- p + P(trafo3, condition = "500Density")

#Initialize parameters and make prediction--------
outerpars <- getParameters(p)
pouter <- log(c(delta_L = 0.1, gamma_on = 0.84, t_on = 6, h = 150)) #logvalues
#plot((g*x*p)(times, pouter))


# Parameter estimation ----------
#read the data
df.Pupae<-read.csv(P_FILE, sep = ",", header = TRUE, row.names = NULL)

#add some small error to avoid divisoin by zero
df.Pupae$sigma<-df.Pupae$sigma + 0.005
data<-as.datalist(df.Pupae,split.by = "condition")
#plot(data) +geom_line()

#fix some parameters
fixed<-c(t_on = log(6))

# Define prior values for parameters
prior<-pouter[setdiff(names(pouter), names(fixed))]

#prior<-pouter
#prior<-structure(rep(0, length(pouter)), names = names(pouter))  

# Set up objective function
obj <- normL2(data, g*x*p) + constraintL2(mu =0*prior, sigma = 5)

# Optimize the objective function
myfit<- trust(obj, prior, rinit = 1, rmax = 10, fixed = fixed)

plot((g*x*p)(times, myfit$argument, fixed = fixed), data)

# Explore the parameter space --------
fitlist <- mstrust(obj, center = myfit$argument, fits = 100, cores = 4, sd = 1, samplefun = "rnorm", fixed = fixed)
pars <- as.parframe(fitlist)

#save the results
write.table(pars, file = paste(OUT_DIR, "parameters_msTrust_", STRAIN, ".csv", sep = ""), sep = ";", row.names=FALSE)

#calculate the residuals
residuals<-myCalculateResiduals(pars[1,],g*x*p,data, fixed = fixed)
write.table(residuals, file = paste(OUT_DIR, "residuals_", STRAIN, ".csv", sep = ""), sep = ";", row.names=FALSE)

#plot the residuals
pdf(file=paste(OUT_FIG,"residuals_", STRAIN,".pdf", sep = ""))
myPlotResiduals(residuals)
dev.off()

#plotValues(subset(pars, converged))
#plotPars(subset(pars, converged))

controls(g, NULL, "attach.input") <- TRUE
prediction <- predict(g*x*p, 
                      times = 0:18, 
                      pars = subset(pars,converged), 
                      fixed = fixed,
                      data = data)

plot_mstrust<-ggplot(subset(prediction, name !="gamma" & name!= "P"), aes(x = time, y = value, color = .value, group = .value)) +facet_grid(condition~name, scales = "free") + geom_line() + geom_point(data = attr(prediction, "data")) +theme_dMod() 

pdf(file=paste(OUT_FIG,"bestPredictions_", STRAIN,".pdf", sep = ""))
plot_mstrust
dev.off()

# Compute the profile likelihood around the optimum -------------
bestfit <- as.parvec(pars)
profiles <- profile(obj, bestfit, names(bestfit), cores = 4, fixed = fixed)

# Take a look at each parameter
pdf(file=paste(OUT_FIG, "PL_", STRAIN, ".pdf", sep = ""))
plotProfile(profiles)
dev.off()

#save the profiles
write.csv(profiles, file = paste(OUT_DIR, "PL_", STRAIN, ".csv", sep = ""), row.names = FALSE)

fittedValues<-confint(profiles)
#transform  the values back to natural scale
fittedValues[,2:4]<-exp(fittedValues[,2:4])
fittedValues<-rbind(fittedValues, data.frame(name = "AICc", value = AICc1(pars[1,],nrow(residuals)), lower = "", upper = " "))
write.csv(fittedValues, file = paste(OUT_DIR,  "fittedParameters_", STRAIN, ".csv",sep = ""), row.names = FALSE)


# Prediction uncertainty and validation profiles ----------------
obj.validation<-datapointL2(name = "Pupae",
                            time = 15,
                            value = "newpoint",
                            sigma = .1,
                            condition = "100Density")

myValidationFit<-trust(obj+obj.validation, parinit = c(newpoint = 1, bestfit), fixed = fixed,rinit = 1, rmax = 10)

profile_prediction <- profile(obj + obj.validation,
                              myValidationFit$argument, "newpoint",
                              fixed = fixed,
                              stepControl = list(stop = "data"),
                              cores =4)

plotProfile(profile_prediction, mode %in% c("data", "validation"))

# Prediction band (prediction uncertainty for several time points) --------------
compute_prediction_band <- function(condition="100Density")
{
  do.call(rbind, lapply(c(5., 6., 7., 8., 9., 10.,11.,12., 13.,14.,15.,16.,17.), function(t) {
    
    cat("Computing prediction profile for t =", t, "and condition =", condition, "\n")
    obj.validation <- datapointL2(name = "Pupae",
                                  time = t,
                                  value = "newpoint",
                                  sigma = 1,
                                  condition = condition)
    
    myfit <- trust(obj + obj.validation,
                   parinit = c(newpoint = 1, bestfit),
                   fixed = fixed,
                   rinit = 1, rmax = 10)
    
    profile_prediction <- profile(obj + obj.validation,
                                  myfit$argument, "newpoint",
                                  fixed = fixed,
                                  cores =4)
    
    d1 <- confint(profile_prediction, val.column = "value")
    # Output
    data.frame(time = t, condition = condition, name = "Pupae",  d1[-1])
  }))
}
  
#predic for three conditions
prediction_band<-do.call(rbind, lapply(c("100Density", "250Density", "500Density"), function(condition)
{
  compute_prediction_band(condition)
}))

myPrediction<-(g*x*p)(times, bestfit, fixed = fixed)
plot(myPrediction, data) + 
  geom_ribbon(aes(x = time, ymin = lower, ymax = upper,fill = as.factor(condition)), 
              data = prediction_band,
              lty = 0, alpha = .3) +scale_fill_dMod() +theme_dMod(base_size = 24)



#save the model predictions with best profile-
write.csv(prediction_band, file = paste(OUT_DIR,  "predictionBand_", STRAIN, ".csv",sep = ""), row.names = FALSE)

pdf(file=paste(OUT_FIG, "bestPredictionsWrap_", STRAIN,".pdf", sep = ""), width = 10, height = 10)
plot(myPrediction, data) + 
  geom_ribbon(aes(x = time, ymin = lower, ymax = upper,fill = as.factor(condition)), 
              data = prediction_band,
              lty = 0, alpha = .3) +scale_fill_dMod() +theme_dMod(base_size = 24)
dev.off()
