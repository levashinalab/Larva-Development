##################################################################
#####  This is a model to fit the data on larva development ####
#####  to a simple ODE model of larva growth                #####
#####  Created by: Paola Carrillo-Bustamante                #####
#####  05 July 2018                                         #####
##################################################################


#source grind and the models
source("~/Nextcloud/Documents/Resources/Modelling/grindR/grindR.R")
source("~/Documents/Projects/LarvaeDevelopment/src/Larva-Development/larvaDevelopmentModels.R")

# myfit() calls fit() with optimized parameters:
# lower=0: only positive parameters
# fun=log1p: log(1+x) transformation
# ymin , ymax , log="y", tmax: parameters for plotting result:
myfit <- function(data ,w=who ,...) {
  return(fit(datas=data , ymin=1, ymax=1e8, log="y", tmax=100 , who=w, solution=T,
             lower=0, fun=log1p , ...))
}
