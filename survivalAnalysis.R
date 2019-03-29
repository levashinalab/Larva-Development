##This script calculates the Kaplan-Meier curves for survival data 
## Created by Paola Carrillo-Bustamante 
## 24-10-2018

#remove old values to avoid trouble
rm(list=ls())

#load the necessary libraries and files. You might need to install the packages first
require(plyr) #necessary for data processing
require(dplyr)
require(gdata) #necessary to read xls files
library(survival)
source("https://bioconductor.org/biocLite.R")

library(ggplot2)
library(survminer)
library(survtools)
library(flexsurv)
#source('~/Documents/Projects/Malaria/modeling/ABM/src/RScripts/ggplotThemes.R') #update the path to where you saved this file

TYPE<- "survival"

IN_DIR<-paste('/Volumes/abt.levashina/Project Development_AW_PCB_PS/rawData/', TYPE, '/', sep = "")

OUT_FILE<-paste("/Volumes/abt.levashina/Project Development_AW_PCB_PS/figures/", TYPE,"/",Sys.Date(),"_survival.pdf",sep = "")

OUT_FILE_HL<-paste("/Volumes/abt.levashina/Project Development_AW_PCB_PS/figures/",TYPE,"/", Sys.Date(),"_survival_halfLife.pdf",sep = "")

OUT_FILE_F<-paste("/Volumes/abt.levashina/Project Development_AW_PCB_PS/figures/",TYPE,"/", Sys.Date(),"_survival_Fit.pdf",sep = "")

OUT_FILE_HAZARD<-paste("/Volumes/abt.levashina/Project Development_AW_PCB_PS/analysis/", TYPE,"/",Sys.Date(),"_predicted_hazard.csv",sep = "")

#set the current directory to IN_DIR
setwd(IN_DIR)

#save all files wihtin the directory
nm <- list.files(path=IN_DIR)


#concatenate all data frames in the directory into one-------
survivalData<-do.call(rbind, lapply(nm, function(x) read.xls(x, sheet = 1)))

##extract relevant information ----
## 1. comparison Densities
##survival fit 
fit<-survfit(Surv(Day, Status) ~ Density  + Temperature. + Type, data= subset(survivalData, Type !="p")
             )

#calculate median survival
fit_med<-surv_median(fit, combine = TRUE)

density<-c(rep(100,6), rep(250,6),rep(500,6))
temp<-rep(c(24,24,26,26,28,28),3)
type<-rep(c("f", "m"),9)
halfLife<-data.frame(density, temp, type, fit_med)

#plot the half life----
plot_hl1<-ggplot(halfLife, aes(x = as.factor(temp), group = as.factor(temp), y = median,colour = as.factor(temp))) +geom_point(size = 5)+ geom_errorbar(aes(ymin = lower, ymax = upper)) +facet_grid(type~density) +ylim(c(10,30)) + theme_bw(base_size = 24) + ylab("Median survival") +xlab("Temperature") +theme(legend.position = "none") +scale_color_manual(values = c("orange","darkgreen", "darkblue")) 

plot_hl2<-ggplot(halfLife, aes(x = as.factor(density), group = as.factor(density), y = median,colour = as.factor(density))) +geom_point(size = 5)+ geom_errorbar(aes(ymin = lower, ymax = upper)) +facet_grid(type~temp) +ylim(c(10,30)) + theme_bw(base_size = 24) + ylab("Median survival") +xlab("Density") +theme(legend.position = "none") +scale_color_manual(values = c("black", "red", "blue")) 

### plot the survival curves -----
plot_t<-ggsurvplot(fit, data = subset(survivalData, Type !="p"), pval=TRUE, pval.method = TRUE,  ggtheme = theme_bw(base_size = 24), font.family = "Helvetica", conf.int = TRUE,color = "Density", palette = c("black", "red", "blue"), facet.by = c("Temperature.","Type"))

plot_d<-ggsurvplot(fit, data = subset(survivalData, Type !="p"), pval=TRUE, pval.method = TRUE,  ggtheme = theme_bw(base_size = 24), font.family = "Helvetica", conf.int = TRUE,color = "Temperature.", palette = c("orange", "darkgreen", "darkblue"), facet.by = c("Density","Type"))

#save the plots----
pdf(OUT_FILE, width = 10, height = 12)
plot_d
plot_t
dev.off()

pdf(OUT_FILE_HL, width = 10, height = 8)
plot_hl1
plot_hl2
dev.off()



# Fit parametric survival models -------------------------------------------
#fit 6 standard distributions
#this fits the distributions and collects the model fit objects in a list
df.short<-subset(survivalData, Type =="f" & Density == 500)
df.short$Temperature.<-as.character(df.short$Temperature.)
df.short$Temperature.<-factor(df.short$Temperature., levels = c("28", "26", "24"))

psm.list <- list(
  #exp = flexsurvreg(Surv(Day, Status)~Temperature. , data= df.short, dist = 'exp'),
  #weibull = flexsurvreg(Surv(Day, Status) ~Temperature. , data= df.short, dist = 'weibull'),
  gompertz = flexsurvreg(Surv(Day, Status)~ Temperature. , data= df.short, dist = 'gompertz')
  #llog = flexsurvreg(Surv(Day, Status)~Temperature. , data= df.short, dist = 'llogis'),
  #lnorm = flexsurvreg(Surv(Day, Status)~ Temperature. , data= df.short, dist = 'lnorm'),
  #gengamma = flexsurvreg(Surv(Day, Status)~Temperature. , data= df.short, dist = 'gengamma')
)

#get the fitted survival curve for each distribution and store the results in a list
#this is equivalent to 'summary(model.fit.obj, type = 'survival', tidy = TRUE)' for each distribution
#Each element of the list a data frame with the results of the corresponding model
psm.summ <- lapply(psm.list, summary, type = 'survival', tidy = TRUE, t = seq(0,60))

#Edit the group column to label all the permutations of group and model
for(i in 1:length(psm.summ)){
  psm.summ[[i]]$Temperature. <- paste0(names(psm.summ)[i], ' ', psm.summ[[i]]$Temperature.)
}


#short fit only for 500 density
fit.short<-survfit(Surv(Day, Status) ~ Temperature., data= df.short)
#ggsurvplot2(fit = fit.short, psm.curves =psm.summ, data = df.short, xval = 'time', groups = 'Temperature.',
#            censor = FALSE, risk.table = TRUE, risk.table.height = 0.2, risk.table.y.text.col = FALSE, xlim=c(0,40),
#            title = 'Survival Adult Mosquitoes', conf.int = TRUE,conf.int.alpha = 0.2,
#            legend.title = 'Temperature', legend.labs =  c('24', '26', '28'),
#            palette = rep(c("darkblue", "darkgreen", "orange"),length(psm.summ)+1) #use this to control colour of lines
#)

#-------Now repeat the same for T=28 ----
df.short.temp<-subset(survivalData, Type =="f" & Temperature. ==28)
df.short.temp$Density<-as.character(df.short.temp$Density)

psm.list.temp <- list(
 # exp = flexsurvreg(Surv(Day, Status)~Density , data= df.short.temp, dist = 'exp'),
 # weibull = flexsurvreg(Surv(Day, Status) ~Density , data= df.short.temp, dist = 'weibull'),
#  gompertz = flexsurvreg(Surv(Day, Status)~ Density , data= df.short.temp, dist = 'gompertz'),
#  llog = flexsurvreg(Surv(Day, Status)~Density , data= df.short.temp, dist = 'llogis'),
#  lnorm = flexsurvreg(Surv(Day, Status)~ Density , data= df.short.temp, dist = 'lnorm'),
  gengamma = flexsurvreg(Surv(Day, Status)~Density , data= df.short.temp, dist = 'gengamma')
)

#get the fitted survival curve for each distribution and store the results in a list
#this is equivalent to 'summary(model.fit.obj, type = 'survival', tidy = TRUE)' for each distribution
#Each element of the list a data frame with the results of the corresponding model
psm.summ.temp <- lapply(psm.list.temp, summary, type = 'survival', tidy = TRUE, t = seq(0,60))

#Edit the group column to label all the permutations of group and model
for(i in 1:length(psm.summ.temp)){
  psm.summ.temp[[i]]$Density <- paste0(names(psm.summ.temp)[i], ' ', psm.summ.temp[[i]]$Density)
}

#short fit only for this Temperature
fit.short.temp<-survfit(Surv(Day, Status) ~ Density, data= df.short.temp)

##calculate now the hazard per Density
hazard<-lapply(psm.list.temp, summary, type = 'hazard', tidy = TRUE, t = seq(0,40))
hazard_df<-data.frame(hazard)
names(hazard_df)<-c("Time", "Hazard", "lcl", "ucl", "Density")
hazard_df[is.na(hazard_df)]<-0

write.csv2(hazard_df, file = OUT_FILE_HAZARD, row.names = FALSE)


#save the plots
pdf(OUT_FILE_F, width = 10, height = 8)
ggsurvplot2(fit = fit.short.temp, psm.curves =psm.summ.temp, data = df.short.temp, xval = 'time', groups = 'Density',
            censor = FALSE, risk.table = FALSE, risk.table.height = 0.2, risk.table.y.text.col = FALSE, xlim=c(0,40),
            title = 'Survival Adult Mosquitoes', conf.int = TRUE,conf.int.alpha = 0.2,
            legend.title = 'Density', legend.labs =  c('100', '250', '500'), 
            pval=TRUE, pval.method = TRUE, font.family = "Helvetica", font.x = 24, font.y = 24, font.tickslab = 24, font.legend = 24,palette = rep(c("black", "red", "blue"),length(psm.summ.temp)+1) #use this to control colour of lines
)
dev.off()