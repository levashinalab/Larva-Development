#remove old values to avoid trouble
rm(list=ls())
#load the necessary libraries and files

suppressPackageStartupMessages(require(ggplot2, quietly = TRUE))
suppressPackageStartupMessages(require(plyr, quietly = TRUE))
source('~/Documents/Projects/Malaria/modeling/ABM/src/RScripts/ggplotThemes.R')


IN_FILE<-'~/Documents/Projects/LarvaeDevelopment/raw_Data/2018_06_28_development-data-28.csv'


df.orig<-read.csv(IN_FILE, sep = ";", header = TRUE, row.names = NULL)

#calculate the average pupation time per ex. repeat, per pan
p.time.average<-ddply(df.orig, .(Strain, Density, Ex.Repeat, Pan), function(X){
  ifelse(sum(X$Pupae)==0, f_p<-0, f_p<-sum(X$Day * X$Pupae / sum(X$Pupae)))
  data.frame(f_p)
})


#calculate first the average per experiment
p.time.double.average<-ddply(p.time.average, .(Strain,Density, Ex.Repeat), function(X)
{
  mean.exp<-mean(X$f_p)
  sd.exp<-sd(X$f_p)
  median.exp<-median(X$f_p)
  data.frame(mean.exp, sd.exp, median.exp)
})

#write the data and save ut to a csv file

### to migrate to a plotting file
##this is the plot of the pulled data
pl1<-ggplot(p.time.average, aes(x = " ", y = f_p)) + stat_boxplot(width = 0.5) + facet_grid(Strain~Density) + stat_summary(fun.y = mean, geom = "point", shape = 1, size = 3) + geom_point(aes(colour = as.factor(Ex.Repeat)),position = position_jitter(width = 0.05), size = 1.5) +ylim(c(0,15))+theme(legend.position = "none", axis.title.x = element_blank()) +ylab("Average pupation time") + basic_theme

#this is the plot of the medians
pl2<-ggplot(p.time.double.average, aes(x = " ", y = median.exp)) + stat_boxplot(width = 0.5) + facet_grid(Strain~Density) + stat_summary(fun.y = mean, geom = "point", shape = 1, size = 3) + geom_point(aes(colour = as.factor(Ex.Repeat)),position = position_jitter(width = 0.05), size = 1.5)+ylim(c(6.5,15)) +theme(legend.position = "none", axis.title.x = element_blank())+ylab("Average pupation time") +basic_theme

#this is the plot of the means
pl3<-ggplot(p.time.double.average, aes(x = " ", y = mean.exp)) + stat_boxplot(width = 0.5) + facet_grid(Strain~Density) + stat_summary(fun.y = mean, geom = "point", shape = 1, size = 3) + geom_point(aes(colour = as.factor(Ex.Repeat)),position = position_jitter(width = 0.05), size = 2)+ylim(c(0,15))+theme(legend.position = "none", axis.title.x = element_blank())+ylab("Average pupation time") +basic_theme


ggplot(p.time.double.average, aes(x = Density, y = median.exp))  +geom_smooth(method = "lm") +theme_bw(base_size = 24)+ylab("Average pupation time") +theme(legend.position = "none")+ geom_point(aes(colour = as.factor(Ex.Repeat)), size = 3) +facet_wrap(~Strain)




##note to myself: using the emans doesn't make much sense because I have that outlier for 250 where nothing pupated. If i make the analysis of the median of the pans per experiment, that outlier doesn't have such a large effect -> i don't really know whether that's ok to do or not. 
