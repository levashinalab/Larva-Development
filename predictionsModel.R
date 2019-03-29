#  ODE model of larval development #
#  Predictions
#  Paola Carrillo-Bustamante
#  Max Planck Institute for Infection Biology
#  25-07-2018


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
OUT_DIR<-paste("~/Documents/Projects/LarvaeDevelopment/data_fitting/", Sys.Date(), "/",sep = "")
OUT_FIG<-paste("~/Documents/Projects/LarvaeDevelopment/figures/fitting/", Sys.Date(), "/",sep = "")
STRAIN<-"RsgG_fixed_growth_exp_"


ifelse(!dir.exists(OUT_DIR), dir.create(OUT_DIR), FALSE)
ifelse(!dir.exists(OUT_FIG), dir.create(OUT_FIG), FALSE)


#define the model in dMod style----
#define the ode Model (see the larvaDevelopmentModels.R for the exact definition of the models)
f<-f3
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


#Define parameter transformation for the different experimental conditions -----
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
trafo1 <- trafo2<- trafo3<-trafo4<-trafo5<-trafo

# Set the density conditions
trafo1["L"] <- "20"
trafo2["L"] <- "100"
trafo3["L"] <- "250"
trafo4["L"] <- "500"
trafo5["L"] <- "1000"
# Generate parameter transformation functions
p <- NULL
p <- p + P(trafo1, condition = "20Density")
#p <- p + P(trafo2, condition = "100Density")
#p <- p + P(trafo3, condition = "250Density")
#p <- p + P(trafo4, condition = "500Density")
p <- p + P(trafo5, condition = "1000Density")

#Initialize parameters and make prediction--------
outerpars <- getParameters(p)
pouter <- log(c(delta_L = 0.0420949162383354, gamma_on = 0.314917697445421, t_on = 6, h = 148.55970768798))
times<-0:10

#pdf(file=paste(OUT_FIG, "PredictionsDIfferentDensities_", STRAIN,".pdf", sep = ""), width = 8, height = 8)
plot((g*x*p)(times, pouter)) +theme_dMod(base_size = 26)
#dev.off()
