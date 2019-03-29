#remove old values to avoid trouble
rm(list=ls())
#load the necessary libraries and files. You might need to install the packages first
require(plyr) #necessary for data processing
require(gdata) #necessary to read xls files
library(ggplot2)
source('~/Documents/Projects/Malaria/modeling/ABM/src/RScripts/ggplotThemes.R')


TYPE<- "infection"

IN_DIR<-paste('/Volumes/abt.levashina/Project Development_AW_PCB_PS/rawData/', TYPE, '/', sep = "")

IN_FILE<-paste("/Volumes/abt.levashina/Project Development_AW_PCB_PS/analysis/",TYPE,"/", Sys.Date(),"_infection_oocysts.csv",sep = "")

IN_FILE_S<-paste("/Volumes/abt.levashina/Project Development_AW_PCB_PS/analysis/",TYPE,"/", Sys.Date(),"_infection_spz.csv",sep = "")

FILE<-paste(IN_DIR, "Infection.xlsx", sep = "")
df.ooc<-read.xls(FILE, sheet = 1)
#df.spz<-read.xls(FILE, sheet = 2)

OUT_FILE<-paste("/Volumes/abt.levashina/Project Development_AW_PCB_PS/figures/", TYPE, "/",Sys.Date(),"_oocysts.pdf",sep = "")

summary_oocysts<-read.csv(IN_FILE, sep = ",")
summary_spz<-read.csv(IN_FILE_S, sep = ",")

ind_plot<-ggplot(df.ooc, aes(x = as.factor(Density), y = Oocyst, group = as.factor(Density), colour = Infection)) +geom_boxplot() + facet_wrap(~Infection) + geom_point(size = 2) +basic_theme +scale_color_manual(values=c("red", "orange", "blue")) +ylab("Number of oocysts") +xlab("Density")

md_plot<-ggplot(summary_oocysts, aes(x = as.factor(Density), y = ooc_median, group = as.factor(Density), colour = Infection)) +geom_point(size = 5) + basic_theme+scale_color_manual(values=c("red", "orange", "blue")) +ylab("Number of oocysts") +xlab("Density") + stat_summary(fun.y = mean, fun.ymin = mean, fun.ymax = mean,geom = "crossbar", width = 0.5)+ylim(c(0,100))

md_short_plot<-ggplot(summary_oocysts, aes(x = as.factor(Density), y = ooc_median_short, group = as.factor(Density), colour = Infection)) +geom_point(size = 5) + basic_theme+scale_color_manual(values=c("red", "orange", "blue")) +ylab("Number of oocysts") +xlab("Density") + stat_summary(fun.y = mean, fun.ymin = mean, fun.ymax = mean,geom = "crossbar", width = 0.5)+ylim(c(0,100))

prev_plot<-ggplot(summary_oocysts, aes(x = as.factor(Density), y = prev*100, group = as.factor(Density), colour = Infection)) +geom_point(size = 5) + basic_theme+scale_color_manual(values=c("red", "orange", "blue")) +ylab("Prevalence %") +xlab("Density")+ stat_summary(fun.y = mean, fun.ymin = mean, fun.ymax = mean,geom = "crossbar", width = 0.5) +ylim(c(0,100))

plot_spz<-ggplot(summary_spz, aes(x = as.factor(Density), y = mean_sporo, group = Density, colour = Infection))  +scale_y_log10()+geom_point(size = 5) + basic_theme+scale_color_manual(values=c("red", "orange", "blue")) +ylab("Sporozoites") +xlab("Density")+ stat_summary(fun.y = mean, fun.ymin = mean, fun.ymax = mean,geom = "crossbar", width = 0.5)


pdf(OUT_FILE, width = 9, height = 9)
ind_plot
md_plot
gridExtra::grid.arrange(prev_plot,md_short_plot, nrow = 2)
plot_spz
dev.off()



