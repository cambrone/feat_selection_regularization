###########################################################################################
# TASK: Explore School Proficiency Data
# Author: Andres Cambronero
# Project: Feature Selection via Regulatization: School Proficiency Case
# Date Started: June 5, 2018
# Latest Update: July 20, 2018
###########################################################################################


#clean environment
rm(list=ls())


#libraries
library(dplyr)
library(stringr)
library(ggplot2)
library(gridExtra)
library(GGally)
library(cowplot)
library(corrplot)
library(factoextra)

#set working directory
setwd("~/Desktop/summer_projects/NY_school_performance/data")

#load imbalance training data
prof_data<-read.csv("imbalanced_train.csv", colClasses = "character")


# change class of columns 6:30 of object prof_data from character to numeric
prof_data[,6:30]<-sapply(prof_data[,6:30], as.numeric)

#change class of columns CHARTER to character
prof_data$CHARTER<-as.character(prof_data$CHARTER)

#change levels of CHARTER from 1 to Charter and 0 to Not Charter
prof_data$CHARTER[which(prof_data$CHARTER=="1")]<-"Charter"
prof_data$CHARTER[which(prof_data$CHARTER=="0")]<-"Not Charter"

#change class of CHARTER from character to factor
prof_data$CHARTER<-as.factor(prof_data$CHARTER)

#extract relevant columns from prof_data
correlations<-prof_data[, c("PER_FREE_LUNCH", "PER_ECDIS", "PER_SUSPENSIONS", "EXP_PER_ST",
                            "PER_NOT_HQ","PER_BLACK", "PER_HISP", "PER_ASIAN", "PER_WHITE",
                            "VIOL_CRIME_PER_CITIZEN", "PROP_CRIME_PER_CITIZEN")]

#obtain correlation matrix
M<-cor(correlations)

#create correlation heatmap 
corrplot.mixed(M, number.cex = .7, tl.cex=0.5, tl.pos = "lt", tl.col = "black")


#plot distribution of original numeric variables and include correlations
ggpairs(correlations, diag=list(continous=wrap('barDiag', gridLabelSize=3)),
                              lower = list(continuous = wrap("points", alpha = 0.3, size=0.1), 
                                           combo = wrap("dot", alpha = 0.3, size=0.1) ), labelSize=1
) + theme(text = element_text(size=5),
          axis.text.x = element_text(angle=40, hjust=1, size = 5),
          axis.title.y = element_text(angle=60, hjust=1, size = 5))

#################################################################################
# Graphs below show differences in distributions between levels of proficiency
#################################################################################
#PERCENT FREE LUNCH
#get mean of PER_FREE_LUNCH for levels of proficiency
aggregate(prof_data$PER_FREE_LUNCH, list(prof_data$PROF_LEVEL), mean)

#create box plot of free lunch by proficiency levels
box_free_lunch<-ggplot(prof_data, aes(x=PROF_LEVEL, y=PER_FREE_LUNCH, fill=PROF_LEVEL)) +
  geom_boxplot(alpha=0.5) +
  ylab(label="Pct. Free Lunch") + 
  xlab("Proficiency")+
  theme(legend.position='bottom') +
  theme(legend.title=element_blank(), axis.text.x = element_text(size=9))

#create histogram of free lunch by proficiency levels
hist_free_lunch<-ggplot(prof_data, aes(x=PER_FREE_LUNCH, fill=PROF_LEVEL)) + 
  geom_histogram(alpha=0.5)+ 
  ylab(label="Count") +
  xlab("Pct. Free Lunch")+
  facet_grid(. ~ PROF_LEVEL) + 
  theme(legend.position='bottom', legend.title=element_blank(), 
        axis.text.x = element_text(hjust = 1, size=10), 
        strip.text = element_text(size=9))

#place boxplot and histogram in same figure
grid.arrange(box_free_lunch, hist_free_lunch, ncol=2)

#PERCENT ECONOMIC DISADVANTAGED
#get mean of PER_ECDIS for levels of proficiency
aggregate(prof_data$PER_ECDIS, list(prof_data$PROF_LEVEL), mean)

#create boxplot of PER_ECDIS by proficiency levels
box_ecdis<-ggplot(prof_data, aes(x=PROF_LEVEL, y=PER_ECDIS, fill=PROF_LEVEL)) + 
  geom_boxplot(alpha=0.5) +
  ylab(label="Pct. Econ. Dis.") + 
  xlab("Proficiency")+
  theme(legend.position='bottom') +
  theme(legend.title=element_blank(), axis.text.x = element_text(size=9))

#create histogram of PER_ECDIS by proficiency levels
hist_ecdis<-ggplot(prof_data, aes(x=PER_ECDIS, fill=PROF_LEVEL)) + 
  geom_histogram(alpha=0.5)+ 
  ylab(label="Count") +
  xlab("Pct. Econ. Dis.")+
  facet_grid(. ~ PROF_LEVEL) + 
  theme(legend.position='bottom', legend.title=element_blank(), 
        axis.text.x = element_text(size=8), 
        strip.text = element_text(size=9))


#place boxplot and histogram in same figure
grid.arrange(box_ecdis, hist_ecdis, ncol=2)


#PERCENT BLACK
#get mean of PER_BLACK for levels of proficiency
aggregate(prof_data$PER_BLACK, list(prof_data$PROF_LEVEL), mean)

#create boxplot of PER_BLACK by proficiency levels
box_black<-ggplot(prof_data, aes(x=PROF_LEVEL, y=PER_BLACK, fill=PROF_LEVEL)) +
  geom_boxplot(alpha=0.5)+
  ylab(label="Pct. Black") +
  xlab("Proficiency")+
  theme(legend.position='bottom') +
  theme(legend.title=element_blank(), axis.text.x = element_text(size=9))
  

#create histogram of PER_BLACK by proficiency levels
hist_black<-ggplot(prof_data, aes(x=PER_BLACK, fill=PROF_LEVEL)) +
  geom_histogram(alpha=0.5)+
  ylab(label="Count") +
  xlab("Pct. Black")+
  facet_grid(. ~ PROF_LEVEL) + 
  theme(legend.position='bottom', legend.title=element_blank(), 
        axis.text.x = element_text(hjust = 1, size=10), 
        strip.text = element_text(size=9))

#place boxplot and histogram in same figure
grid.arrange(box_black, hist_black, ncol=2)

#PERCENT HISPANIC
#get mean of PER_HISP for levels of proficiency
aggregate(prof_data$PER_HISP, list(prof_data$PROF_LEVEL), mean)

#create boxplot of PER_HISP by proficiency levels
box_hisp<-ggplot(prof_data, aes(x=PROF_LEVEL, y=PER_HISP, fill=PROF_LEVEL)) + 
  geom_boxplot(alpha=0.5)+
  ylab(label="Pct. Hispanic") +
  xlab("Proficiency")+
  theme(legend.position='bottom') +
  theme(legend.title=element_blank(), axis.text.x = element_text(size=9))

#create histogram of PER_HISP by proficiency levels
hist_hisp<-ggplot(prof_data, aes(x=PER_HISP, fill=PROF_LEVEL)) +
  geom_histogram(alpha=0.5)+
  ylab(label="Count") +
  xlab("Pct. Hispanic")+
  facet_grid(. ~ PROF_LEVEL) + 
  theme(legend.position='bottom', legend.title=element_blank(), 
        axis.text.x = element_text(hjust = 1, size=10), 
        strip.text = element_text(size=9))

#place boxplot and histogram in same figure
grid.arrange(box_hisp, hist_hisp, ncol=2)


#PERCENT ASIAN
#get mean of PER_ASIAN for levels of proficiency
aggregate(prof_data$PER_ASIAN, list(prof_data$PROF_LEVEL), mean)

#create boxplot of PER_ASIAN by proficiency levels
box_asian<-ggplot(prof_data, aes(x=PROF_LEVEL, y=PER_ASIAN, fill=PROF_LEVEL)) +
  geom_boxplot(alpha=0.5)+
  ylab(label="Pct. Asian") +
  xlab("Proficiency")+
  theme(legend.position='bottom') +
  theme(legend.title=element_blank(), axis.text.x = element_text(size=9))

#create histogram of PER_ASIAN by proficiency levels
hist_asian<-ggplot(prof_data, aes(x=PER_ASIAN, fill=PROF_LEVEL)) +
  geom_histogram(alpha=0.5)+
  ylab(label="Count") +
  xlab("Pct. Asian")+
  facet_grid(. ~ PROF_LEVEL) + 
  theme(legend.position='bottom', legend.title=element_blank(), 
        axis.text.x = element_text(hjust = 1, size=10), 
        strip.text = element_text(size=9))

#place boxplot and histogram in same figure
grid.arrange(box_asian, hist_asian, ncol=2)

#PERCENT WHITE
#create boxplot of PER_WHITE by proficiency levels
box_white<-ggplot(prof_data, aes(x=PROF_LEVEL, y=PER_WHITE, fill=PROF_LEVEL)) +
  geom_boxplot(alpha=0.5)+
  ylab(label="Pct. White") +
  xlab("Proficiency")+
  theme(legend.position='bottom') +
  theme(legend.title=element_blank(), axis.text.x = element_text(size=9))

#create histogram of PER_WHITE by proficiency levels
hist_white<-ggplot(prof_data, aes(x=PER_WHITE, fill=PROF_LEVEL)) +
  geom_histogram(alpha=0.5)+
  ylab(label="Count") +
  xlab("Pct. White")+
  facet_grid(. ~ PROF_LEVEL) + 
  theme(legend.position='bottom', legend.title=element_blank(), 
        axis.text.x = element_text(hjust = 1, size=10), 
        strip.text = element_text(size=9))

#place boxplot and histogram in same figure
grid.arrange(box_white, hist_white, ncol=2)

#CHARTER
#calculate proportion of schools in each proficiency level is charter
charter <-
  prof_data %>%
  group_by(PROF_LEVEL, CHARTER) %>%
  summarize(count=n())%>%
  group_by(CHARTER) %>%
  mutate(total=sum(count))%>%
  mutate(prop=count/total)

#create bar plot by proficiency level
ggplot(charter, aes(x=PROF_LEVEL, y = prop, fill=PROF_LEVEL))+
  geom_bar(stat = "identity", alpha=0.5) +facet_grid(. ~ CHARTER)+
  ylab(label="Proportion") +
  xlab("Proficiency Status")+
  theme(legend.position='bottom', legend.title=element_blank(), 
        axis.text.x = element_text(hjust = 1, size=10), 
        strip.text = element_text(size=9))



#Violent Crimes
#create boxplot of VIOL_CRIME_PER_CITIZEN by proficiency levels
box_violent<-ggplot(prof_data, aes(x=PROF_LEVEL, y=VIOL_CRIME_PER_CITIZEN, fill=PROF_LEVEL)) +
  geom_boxplot(alpha=0.5)+
  ylab(label="Violent Crime per Cit.") +
  xlab("Proficiency")+
  theme(legend.position='bottom') +
  theme(legend.title=element_blank(), axis.text.x = element_text(size=9))

#create histogram of VIOL_CRIME_PER_CITIZEN by proficiency levels
hist_violent<-ggplot(prof_data, aes(x=VIOL_CRIME_PER_CITIZEN, fill=PROF_LEVEL)) +
  geom_histogram(alpha=0.5)+
  ylab(label="Count") +
  xlab("Violent Crime per Cit.")+
  facet_grid(. ~ PROF_LEVEL) + 
  theme(legend.position='bottom', legend.title=element_blank(), 
        axis.text.x = element_text(hjust = 1, size=10), 
        strip.text = element_text(size=9))

#place boxplot and histogram in same figure
grid.arrange(box_violent, hist_violent, ncol=2)


#Property Crimes
#create boxplot of PROP_CRIME_PER_CITIZEN by proficiency levels
box_prop<-ggplot(prof_data, aes(x=PROF_LEVEL, y=PROP_CRIME_PER_CITIZEN, fill=PROF_LEVEL)) +
  geom_boxplot(alpha=0.5)+
  ylab(label="Property Crime per Cit.") +
  xlab("Proficiency")+
  theme(legend.position='bottom') +
  theme(legend.title=element_blank(), axis.text.x = element_text(size=9))

#create histogram of PROP_CRIME_PER_CITIZEN by proficiency levels
hist_prop<-ggplot(prof_data, aes(x=PROP_CRIME_PER_CITIZEN, fill=PROF_LEVEL)) +
  geom_histogram(alpha=0.5)+
  ylab(label="Count") +
  xlab("Violent Crime per Cit.")+
  facet_grid(. ~ PROF_LEVEL) + 
  theme(legend.position='bottom', legend.title=element_blank(), 
        axis.text.x = element_text(hjust = 1, size=10, angle = 30), 
        strip.text = element_text(size=9))

#place boxplot and histogram in same figure
grid.arrange(box_prop, hist_prop, ncol=2)


#TOTAL POPULATION
#create boxplot of TOTAL_DIST_POP by proficiency levels
box_pop<-ggplot(prof_data, aes(x=PROF_LEVEL, y=log(TOTAL_DIST_POP), fill=PROF_LEVEL)) +
  geom_boxplot(alpha=0.5)+
  ylab(label="Log Population") +
  xlab("Proficiency")+
  theme(legend.position='bottom') +
  theme(legend.title=element_blank(), axis.text.x = element_text(size=9))

#create histogram of TOTAL_DIST_POP by proficiency levels
hist_pop<-ggplot(prof_data, aes(x=log(TOTAL_DIST_POP), fill=PROF_LEVEL)) +
  geom_histogram(alpha=0.5)+
  ylab(label="Count") +
  xlab("Log Population")+
  facet_grid(. ~ PROF_LEVEL) + 
  theme(legend.position='bottom', legend.title=element_blank(), 
        axis.text.x = element_text(hjust = 1, size=8, angle = 30), 
        strip.text = element_text(size=9))

#place boxplot and histogram in same figure
grid.arrange(box_pop, hist_pop, ncol=2)

#Plot PER_WHITE by index of observations
index_white<-ggplot(aes(x = as.numeric(row.names(prof_data)), y = PER_WHITE), data = prof_data) +
  ylab(label="Pct. White") +
  xlab("Index")+
  ggtitle("Pct. White by Index")+
  geom_point(aes(colour=PROF_LEVEL), alpha=0.5, size=0.5) + 
  guides(colour = guide_legend(override.aes = list(size=3, alpha=0.5)))+
  theme(legend.position='bottom', legend.title=element_blank(), 
        axis.text.x = element_text(size=10), 
        strip.text = element_text(size=9))

#Plot PER_BLACK by index of observations
index_black<-ggplot(aes(x = as.numeric(row.names(prof_data)), y = PER_BLACK), data = prof_data) +
  ylab(label="Prct Black") +
  xlab("Index")+
  ggtitle("Pct. Black by Index")+
  geom_point(aes(colour=PROF_LEVEL), alpha=0.5, size=0.5)+
  guides(colour = guide_legend(override.aes = list(size=3, alpha=0.5)))+
  theme(legend.position='bottom', legend.title=element_blank(), 
        axis.text.x = element_text(size=10), 
        strip.text = element_text(size=9))

#Plot PER_HISP by index of observations
index_hisp<-ggplot(aes(x = as.numeric(row.names(prof_data)), y = PER_HISP), data = prof_data) +
  ylab(label="Pct. Hisp.") +
  xlab("Index")+
  ggtitle("Pct. Hisp. by Index")+
  geom_point(aes(colour=PROF_LEVEL), alpha=0.5, size=0.5) + 
  guides(colour = guide_legend(override.aes = list(size=3, alpha=0.5)))+
  theme(legend.position='bottom', legend.title=element_blank(), 
        axis.text.x = element_text(size=10), 
        strip.text = element_text(size=9))

#Plot PER_ASIAN by index of observations
index_asian<-ggplot(aes(x = as.numeric(row.names(prof_data)), y = PER_ASIAN), data = prof_data) +
  ylab(label="Pct. Asian") +
  xlab("Index")+
  ggtitle("Pct. Asian by Index")+
  geom_point(aes(colour=PROF_LEVEL), alpha=0.5, size=0.5) + 
  guides(colour = guide_legend(override.aes = list(size=3, alpha=0.5)))+
  theme(legend.position='bottom', legend.title=element_blank(), 
        axis.text.x = element_text(size=10), 
        strip.text = element_text(size=9))

#place index plots in the same figure
grid.arrange(index_black,index_white, index_hisp, index_asian, ncol=2, nrow=2)





