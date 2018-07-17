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

#define the model in dMod style----
#define the ode Model
f<-eqnvec(
  L = "- delta_L*L - gamma*L*(1-(L/(L+h)))",
  P = "gamma*L*(1-(L/(L+h)))",
  gamma = 0
)

myevent <- eventlist(var = "gamma", time = "t_on", value = "gamma_on" , method = "replace")

#ODE model
model0<-odemodel(f, modelname = "larvaGrowth",compile = TRUE, events = myevent)

#Prediction function
x<-Xs(model0)

#Define observables and generate observation function g ------
observables<-eqnvec(
  Pupae = "P"#,
  #Larvae = "L"
)

# Generate observation functions
g <- Y(observables, x, compile = TRUE, modelname = "obsfn", attach.input = FALSE)

# Make a prediction of the observables based on initial parameter values
parTest<-c(L = 100, P = 0, gamma = 0, delta_L = 0.1, h = 100, gamma_on = 0.9, t_on = 7)
times <- seq(0, 18, len = 100)
prediction <- (g*x)(times, parTest)
plot(prediction)


#Define parameter transformation for two experimental conditions -----
# Get all parameters
innerpars <- getParameters(g*x)

# Start with the identity transformation
trafo <- repar("x~x", x = innerpars)

# Set some initial value parameters
trafo <- repar("x~100", x = "L", trafo)
trafo <- repar("x~0", x = "P", trafo)
trafo <- repar("x~0", x = "gamma", trafo)

# Explicit log-transform of all parameters
trafo <- repar("x~exp(x)", x = innerpars, trafo)

## Split transformation into two
trafo1 <- trafo2<- trafo3<-trafo

# Set the degradation rate in the first condition to 0
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
pouter <- log(c(delta_L = 0.1, gamma_on = 0.9, t_on = 7, h = 100))
plot((g*x*p)(times, pouter))


#Parameter estimation----
#### Preparing the data ->to migrate to a spearte script. Here I should only load the data!
IN_FILE<-"~/Documents/Projects/LarvaeDevelopment/analysis/2018_06_28_cumulative_pupae_mean.csv"
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
name<-rep("Pupae", nrow(tmp.df))
value<-subset(tmp.df, Strain =="RsgG", select = "P_mean")
sigma<-subset(tmp.df, Strain =="RsgG", select = "P_se")
condition<-subset(tmp.df, Strain =="RsgG", select = "Density")

#rename condition to match the above definition
condition[condition=="100",]<-"100Density"
condition[condition=="250",]<-"250Density"
condition[condition=="500",]<-"500Density"

df.Pupae<-cbind(time, name, value, sigma, condition)
names(df.Pupae)<-c("time", "name", "value", "sigma", "condition")
######

#add some small error to avoid divisoin by zero
df.Pupae$sigma<-df.Pupae$sigma + 0.005
data<-as.datalist(df.Pupae,split.by = "condition")
plot(data)

# Define prior values for parameters
prior <- structure(rep(0, length(pouter)), names = names(pouter))

# Set up objective function
obj <- normL2(data, g*x*p) + constraintL2(mu = prior, sigma = 10)

# Optimize the objective function
myfit <- trust(obj, exp(pouter), rinit = 1, rmax = 10)

plot((g*x*p)(times, myfit$argument), data)

# Compute the profile likelihood around the optimum
bestfit <- myfit$argument
profiles <- profile(obj, bestfit, names(bestfit), limits = c(-10, 10), cores = 4)

# Take a look at each parameter
plotProfile(profiles)


##STILL TO DO:  explore the parameter space. double check that the log-transfomration is correct, too. Plot the predictions