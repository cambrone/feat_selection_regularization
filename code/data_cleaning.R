###########################################################################################
# TASK: Clean and Merge Education data files
# Author: Andres Cambronero
# Project: Feature Selection via Regulatization: School Proficiency Case
# Date Started: June 1, 2018
# Latest Update: July 20, 2018
###########################################################################################


#############################################################################
# SUBTASK 1: Create response variable from English and Math scores 
# SUBTASK 2: MERGE WITH RELEVANT IDENTIFYING DATA
# SUBTASK 3: MERGE WITH RELEVANT DEMOGRAPHIC DATA
# SUBTASK 4: MERGE prof_data with Suspensions data
# SUBTASK 5: MERGE prof_data with class size data
# SUBTASK 5: MERGE prof_data with BEDS data 
# SUBTASK 7: MERGE prof_data with Staff  data  
# SUBTASK 8: MERGE prof_data with educational attainment in district  data
# SUBTASK 9: MERGE prof_data with Economic attainment in district  data 
# SUBTASK 10: MERGE prof_data with school financial  
# SUBTASK 11: Merge prof_data with teacher scores
# SUBTASK 12: write-out csv file   
#############################################################################

#clean environment
rm(list=ls())

#set working directory
setwd("~/Desktop/summer_projects/NY_school_performance/data")


#libraries
library(dplyr)
library(reshape2)
library(tidyr)
library(stringr)
library(truncnorm)

#load institution groupings
inst_group<-read.csv("Institution Grouping.csv", colClasses = "character", na.strings = c("","s"))

#load math data
math_3<-read.csv("Math3 Subgroup Results.csv", colClasses  = "character", na.strings = c("","s"))
math_4<-read.csv("Math4 Subgroup Results.csv", colClasses  = "character", na.strings = c("","s"))
math_5<-read.csv("Math5 Subgroup Results.csv", colClasses  = "character", na.strings = c("","s"))
math_6<-read.csv("Math6 Subgroup Results.csv", colClasses  = "character", na.strings = c("","s"))
math_7<-read.csv("Math7 Subgroup Results.csv", colClasses  = "character", na.strings = c("","s"))
math_8<-read.csv("Math8 Subgroup Results.csv", colClasses  = "character", na.strings = c("","s"))

#load English data
eng_3<-read.csv("ELA3 Subgroup Results.csv", colClasses = "character", na.strings = c("","s"))
eng_4<-read.csv("ELA4 Subgroup Results.csv", colClasses = "character", na.strings = c("","s"))
eng_5<-read.csv("ELA5 Subgroup Results.csv", colClasses = "character", na.strings = c("","s"))
eng_6<-read.csv("ELA6 Subgroup Results.csv", colClasses = "character", na.strings = c("","s"))
eng_7<-read.csv("ELA7 Subgroup Results.csv", colClasses = "character", na.strings = c("","s"))
eng_8<-read.csv("ELA8 Subgroup Results.csv", colClasses = "character", na.strings = c("","s"))



#################################################################
#TASK 1: Create response variable from English and Math scores  #
#################################################################
#keep schools public schools
inst_group<-inst_group[which(inst_group$GROUP_NAME=="Public School"),]

#function to average years for vulnerable populations in inst_group object
mean_years<-function(data){
  vulnerable<-subset(data, SUBGROUP_NAME=="Asian or Native Hawaiian/Other Pacific Islander"|
                       SUBGROUP_NAME=="Black or African American"|
                       SUBGROUP_NAME=="Economically Disadvantaged"|
                       SUBGROUP_NAME=="Non-English Language Learners"|
                       SUBGROUP_NAME=="Hispanic or Latino"|
                       SUBGROUP_NAME=="Multiracial"|
                       SUBGROUP_NAME=="Students with Disabilities"|
                       SUBGROUP_NAME=="American Indian or Alaska Native")
  
  vulnerable$MEAN_SCORE<-as.numeric(vulnerable$MEAN_SCORE)
  vulnerable<-vulnerable[vulnerable$ENTITY_CD %in% inst_group$ENTITY_CD,
                 c("ENTITY_CD", "ENTITY_NAME", "YEAR", "SUBGROUP_NAME", "MEAN_SCORE")]
  
  vulnerable<-
    vulnerable%>%
    group_by(ENTITY_CD, SUBGROUP_NAME)%>%
    mutate(MEAN_Y=mean(MEAN_SCORE, na.rm = T))
  
  class(vulnerable)<-"data.frame"
  
  vulnerable$YEAR<-NULL
  vulnerable$MEAN_SCORE<-NULL
  vulnerable<-vulnerable[!duplicated(vulnerable), ]
  
  return(vulnerable)
}


#function to assign a group a "proficient" (1) or "not proficient" (-1) score
proficient_grp<-function(data, level){
  data$MEAN_Y[(which(data$MEAN_Y=="NaN"))]<-NA
  data$GROUP_LEVEL<-ifelse(data$MEAN_Y<level, -1, 1)
  
  data$MEAN_Y<-NULL
  
  return(data)
}
    

#math grade 3
#valid range 137-397
#proficient above 314
math_3<-mean_years(math_3)
math_3<-proficient_grp(math_3, 314)



#math grade 4 
#valid range 137-405
#proficient above 314
math_4<-mean_years(math_4)
math_4<-proficient_grp(math_4, 314)



#math grade 5 
#valid range 127-415
#proficient 319
math_5<-mean_years(math_5)
math_5<-proficient_grp(math_5, 319)


#math grade 6 
#valid range 125-411
#proficient above 318
math_6<-mean_years(math_6)
math_6<-proficient_grp(math_6, 318)


#math grade 7
#valid range 124-398
#proficient above 322
math_7<-mean_years(math_7)
math_7<-proficient_grp(math_7, 322)



#math grade 8
#valid range 124-400
#proficient above 322
math_8<-mean_years(math_8)
math_8<-proficient_grp(math_8, 322)



#english grade 3
#valid range 148-423
#proficient above 320
eng_3<-mean_years(eng_3)
eng_3<-proficient_grp(eng_3, 320)


#english grade 4
#valid range 1139-412
#proficient above 320
eng_4<-mean_years(eng_4)
eng_4<-proficient_grp(eng_4, 320)


#english grade 5
#valid range 116-425
#proficient above 320
eng_5<-mean_years(eng_5)
eng_5<-proficient_grp(eng_5, 320)


#english 6 
#valid range 112-412
#proficient above 320
eng_6<-mean_years(eng_6)
eng_6<-proficient_grp(eng_6, 320)


#English 7
# valid range 103-413
# proficient above 318
eng_7<-mean_years(eng_7)
eng_7<-proficient_grp(eng_7, 318)


# English 8
#valid range 100-417
#proficient above 316
eng_8<-mean_years(eng_8)
eng_8<-proficient_grp(eng_8, 316)


#rbind datasets into one object
scores<-rbind(math_3, math_4, math_5, math_6, math_7, math_8,
              eng_3, eng_4, eng_5, eng_6, eng_7, eng_8)


#average scores over subgroups for english and math
scores<-
  scores%>%
  group_by(ENTITY_CD, SUBGROUP_NAME)%>%
  mutate(GROUP_LEVEL=mean(GROUP_LEVEL, na.rm = T))


#change class and  NaN to NA
class(scores)<-"data.frame"
scores$GROUP_LEVEL[which(scores$GROUP_LEVEL=="NaN")]<-NA


#abbreviate levels in the variable SUBGROUP_NAME
scores$SUBGROUP_NAME[which(scores$SUBGROUP_NAME=="Asian or Native Hawaiian/Other Pacific Islander")]<-"ASN"
scores$SUBGROUP_NAME[which(scores$SUBGROUP_NAME=="Black or African American")]<-"BLK"                      
scores$SUBGROUP_NAME[which(scores$SUBGROUP_NAME=="Economically Disadvantaged")]<-"ECN"                     
scores$SUBGROUP_NAME[which(scores$SUBGROUP_NAME=="Non-English Language Learners" )]<-"ELL"                 
scores$SUBGROUP_NAME[which(scores$SUBGROUP_NAME=="Hispanic or Latino")] <-"HSP"                            
scores$SUBGROUP_NAME[which(scores$SUBGROUP_NAME=="Multiracial")]<-"MULTI"                                   
scores$SUBGROUP_NAME[which(scores$SUBGROUP_NAME=="Students with Disabilities")] <-"SWD"                    
scores$SUBGROUP_NAME[which(scores$SUBGROUP_NAME=="American Indian or Alaska Native")] <-"AMIND"


#remove duplicates
scores<-scores[!duplicated(scores), ]


#change formate from long to wide
scores <- dcast(scores, ENTITY_CD + ENTITY_NAME ~ SUBGROUP_NAME, value.var="GROUP_LEVEL")
scores[is.na(scores)]<-0

##########################
# load demographic data  #
##########################
#load demographic data
demo<-read.csv("Demographic Factors.csv", colClasses = "character", na.strings = c("","s"))

#subset demo to keep schools in object scores and keep relevant columns of demo object
demo_1<-demo[which(demo$ENTITY_CD %in% scores$ENTITY_CD),c("ENTITY_CD",
                                                           "NUM_LEP", "NUM_AM_IND", "NUM_BLACK",
                                                           "NUM_HISP", "NUM_ASIAN", "NUM_Multi", 
                                                           "NUM_SWD", "NUM_ECDIS")]

#change class of columns in demo_1 from character to numertic
demo_1[,2:9]<-sapply(demo_1[,2:9], as.numeric)

#for each ethnic group, average percentage between years
demo_1<-
  demo_1%>%
  group_by(ENTITY_CD)%>%
  summarise_all(funs(mean(., na.rm = T)))


#create TOTAL column that is the row sums for the columns 2:9 in demo_1
demo_1$TOTAL<-rowSums(demo_1[,2:9])


#change for each subgroup express raw number as percentage of total students sampled
demo_1$NUM_ECDIS<-demo_1$NUM_ECDIS/demo_1$TOTAL
demo_1$NUM_LEP<-demo_1$NUM_LEP/demo_1$TOTAL
demo_1$NUM_AM_IND<-demo_1$NUM_AM_IND/demo_1$TOTAL
demo_1$NUM_BLACK<-demo_1$NUM_BLACK/demo_1$TOTAL
demo_1$NUM_HISP<-demo_1$NUM_HISP/demo_1$TOTAL
demo_1$NUM_ASIAN<-demo_1$NUM_ASIAN/demo_1$TOTAL
demo_1$NUM_Multi<-demo_1$NUM_Multi/demo_1$TOTAL
demo_1$NUM_SWD<-demo_1$NUM_SWD/demo_1$TOTAL


#merge object scores with object demo_1 by ENTITY_CD
scores<-merge(scores, demo_1, by="ENTITY_CD")


# create weighted mean measure that incorporates scores for each subgroup and the percent population that 
# that subgroup rpresents in the school 
scores$MEAN<-  scores$ECN*scores$NUM_ECDIS + scores$ELL*scores$NUM_LEP + 
  scores$AMIND*scores$NUM_AM_IND + scores$BLK*scores$NUM_BLACK+ scores$HSP*scores$NUM_HISP +
  scores$ASN*scores$NUM_ASIAN +scores$MULTI*scores$NUM_Multi+ scores$SWD*scores$NUM_SWD 


#Based on weighted mean, create PROF_LEVEL variable. Negative means represent "Not Proficient" and 
#positive means represent proficient schools
scores$PROF_LEVEL<-ifelse(scores$MEAN<0, "Not Proficient", "Proficient")


#keep relevant variables of scores object in prof_data object
prof_data<-scores[,c("ENTITY_CD", "ENTITY_NAME", "PROF_LEVEL")]



#################################################
# TASK 2: MERGE WITH RELEVANT IDENTIFYING DATA  #
#################################################
#load data reading "s" as empty value. "S" represents "suppressed"
id_info<-read.csv("BOCES and N_RC.csv", colClasses = "character", na.strings = c("","s"))


# subset id_info to keep schools that are present in prof_data object and keep 
# relevant columns of id_info
id_info<-id_info[which(id_info$ENTITY_CD %in% prof_data$ENTITY_CD),
                 c("ENTITY_CD", "DISTRICT_CD", "DISTRICT_NAME", 
                   "COUNTY_CD", "COUNTY_NAME")]


# remove duplicates
id_info<-id_info[!duplicated(id_info),]

#merge prof_data  object with id_info object by ENTITY_CD
prof_data<-merge(prof_data, id_info, by="ENTITY_CD")


#reoder the columns in object prof_data
prof_data <- prof_data[c("ENTITY_CD", "ENTITY_NAME",
                         "DISTRICT_CD","DISTRICT_NAME",
                         "COUNTY_CD", "COUNTY_NAME",
                         "PROF_LEVEL")]





#################################################
# TASK 3: MERGE WITH RELEVANT DEMOGRAPHIC DATA  #
#################################################
# subset demo to keep schools that are present in prof_data object and keep 
# relevant columns of demo
demo<-demo[which(demo$ENTITY_CD %in% prof_data$ENTITY_CD),
           c("ENTITY_CD", "PER_FREE_LUNCH", "PER_REDUCED_LUNCH",
              "PER_LEP",  "PER_AM_IND", "PER_BLACK", "PER_HISP",  "PER_ASIAN",
              "PER_WHITE", "PER_Multi", "PER_SWD", "PER_FEMALE", "PER_ECDIS")]


#change class of columns in object demo from character to numeric 
demo[,2:13]<-sapply(demo[,2:13], as.numeric)


#For each school, average each column across years
demo<-
  demo%>%
  group_by(ENTITY_CD)%>%
  summarise_all(funs(mean(., na.rm = T)))


#merge demo object with prof_data objet by ENTITY_CD
prof_data<-merge(prof_data, demo, by="ENTITY_CD")



#################################################
# TASK 4: MERGE prof_data with Suspensions data #
#################################################
#load suspensions data
susp<-read.csv("Attendance and Suspensions.csv", colClasses = "character", na.strings = c("","s"))

# keep relevant columns in susp object 
susp<-susp[,c("ENTITY_CD", "PER_SUSPENSIONS")]

#change column 2 of susp oject from character to numeric
susp[,2]<-as.numeric(susp[,2])

#for each school average across years
susp<-
  susp%>%
  group_by(ENTITY_CD)%>%
  summarise_all(funs(mean(., na.rm = T)))


#keep schools in susp object that are also in prof_data object
susp_1<-susp[which(susp$ENTITY_CD %in% prof_data$ENTITY_CD),]

#merge prof_data and susp objects by ENTITY_CD and keep all rows that are unmatched
prof_data<-merge(prof_data, susp_1, by="ENTITY_CD", all = T)


# create function that uses district information and column mean to complete NAs
complete_col<-function(data,col, top){
  
  #change "NaN" to NA
  data[[col]][which(data[[col]]=="NaN")]<-NA
  
  #extract school code for rows that are empty in a column
  id_nas<-data$ENTITY_CD[which(is.na(data[,col]))]
  
  for (id in id_nas){
    #extract disctrict code from school code (first 8 digits of school code)
    district<-substr(id, start=1, stop=8)
    
    #if a school in loop not proficient
    if (data$PROF_LEVEL[which(data$ENTITY_CD==id)] == "Not Proficient"){
      
      #extract rows of data that are not proficient from data object
      data_not_prof<- data[which(data$PROF_LEVEL=="Not Proficient"),]
      
      #find schools that are not proficient and are in the district of the school in loop 
      data_not_prof<- data_not_prof[grep(district, data_not_prof$ENTITY_CD),]
      
      #if no rows or all are NA break the loop
      if (nrow(data_not_prof)==0 | all(is.na(data_not_prof[[col]]))){
        break
      }
      
      #get the mean and st. dev of the not proficient schools in district
      mean_not_prof<-mean(data_not_prof[[col]], na.rm = T)
      std_not_prof<-sd(data_not_prof[[col]], na.rm = T)
      
      # use mean and st dev in district to draw values from a normal distribution 
      # truncated below at 0 and above at "top"
      data[[col]][which(data$ENTITY_CD==id)] <- rtruncnorm(1, a=0, b=top, mean_not_prof, std_not_prof)
        
    }
    
    
    #if a school in loop is proficient
    if (data$PROF_LEVEL[which(data$ENTITY_CD==id)] == "Proficient"){
      
      #extract proficient schools from data object
      data_prof<- data[which(data$PROF_LEVEL=="Proficient"),]
      
      #find schools in the district
      data_prof<- data_prof[grep(district, data_prof$ENTITY_CD),]
      
      #if number of rows is 0 or all are NA break loop
      if (nrow(data_prof)==0 | all(is.na(data_prof[[col]]))){
        break
      }
      
      #get mean and st. dev schools in the district of school in loop
      mean_prof<-mean(data_prof[[col]], na.rm = T)
      std_prof<-sd(data_prof[[col]], na.rm = T)
      
      #use mean and st dev in district to draw values from a truncated normal distribution 
      data[[col]][which(data$ENTITY_CD==id)] <- rtruncnorm(1, a=0, b=top, mean_prof, std_prof)
      
    }
    
  }
  
  #for remaining NA use column mean
  data[[col]][which(is.na(data[[col]]))]<-rtruncnorm(length(data[[col]][which(is.na(data[[col]]))]), a=0, b=top,
                                                     mean(data[[col]], na.rm = T),
                                                     sd(data[[col]], na.rm = T))
  
  return(data)
}


#complete the PER_SUSPENSIONS column in prof_data object
prof_data<-complete_col(prof_data, "PER_SUSPENSIONS", 100)





#################################################
# TASK 5: MERGE prof_data with class size data  #
#################################################
#load data
size<-read.csv("Average Class Size.csv", colClasses = "character", na.strings = c("","s"))


#keep ENTITY_CD in size object that are also in prof_data object
size<-size[which(size$ENTITY_CD %in% prof_data$ENTITY_CD),]


#change class of columns 4:12 of object size from character to numeric
#average size across all years and school subjects and make 
size$MEAN_S <- rowMeans(sapply(size[,4:12], as.numeric), na.rm=TRUE)

#for each school calculate average
size<-
  size%>%
  group_by(ENTITY_CD)%>%
  mutate(MEAN_CLASS_SIZE=mean(MEAN_S, na.rm = T))

#keep important variables and remove duplicates
size<-size[,c("ENTITY_CD", "MEAN_CLASS_SIZE")]
size<-size[!duplicated(size),]


#merge with prof data object and size object on ENTITY_CD and keep unmatched rows
prof_data<-merge(prof_data, size, by="ENTITY_CD", all = T)

#complete MEAN_CLASS_SIZE column in prof data 
prof_data<-complete_col(prof_data, "MEAN_CLASS_SIZE", Inf)




#################################################
# TASK 6: MERGE prof_data with BEDS data        #
#################################################
#load data
enrollment<-read.csv("BEDS Day Enrollment.csv", colClasses = "character", na.strings = c("","s"))


#keep ENTITY_CD that are in prof_data and relevant columns
enrollment<-enrollment[which(enrollment$ENTITY_CD %in% prof_data$ENTITY_CD), 
                       c("ENTITY_CD","X1", "X2", "X3", "X4", "X5", "X6", "X7",
                         "X8", "X9", "X10","X11", "X12")] 

#change class of columns 2:13 from character to numeric
#sum over the rows of enrollement rows
enrollment$TOTAL<-rowSums(sapply(enrollment[,2:13], as.numeric), na.rm = T)

#for each school average across years across years
enrollment<-
  enrollment%>%
  group_by(ENTITY_CD)%>%
  summarise(MEAN_ENROLL=mean(TOTAL, na.rm = T))


#merge with prof_data object with enrollment object and keep unmatched rows
prof_data<-merge(prof_data, enrollment, all=T)








#################################################
#  TASK 7: MERGE prof_data with Staff  data     #
#################################################
#load data
staff<-read.csv("Staff.csv", colClasses = "character", na.strings = c("","s"))

#keep ENTITY_CD that in object staff and are also in prof_data object and keep relevant columns
staff<-staff[which(staff$ENTITY_CD %in% prof_data$ENTITY_CD),c("ENTITY_CD","NUM_TEACH","PER_NOT_HQ")]

#change class of columns 2:3 from character to numeric
staff[,2:3]<-sapply(staff[,2:3], as.numeric)


#for each school and for each column average
staff<-
  staff%>%
  group_by(ENTITY_CD)%>%
  summarise_all(funs(mean(., na.rm = T)))


#merge with prof_data object and staff object by ENTITY_CD
prof_data<-merge(prof_data, staff, by="ENTITY_CD")


#complete NUM_TEACH and PER_NOT_HQ using function 
prof_data<-complete_col(prof_data,"NUM_TEACH", Inf)
prof_data<-complete_col(prof_data,"PER_NOT_HQ", 100)

#make student teacher ratio
prof_data$STUD_TEACH_RATIO<-prof_data$MEAN_ENROLL/prof_data$NUM_TEACH

#drop NUM_TEACH
prof_data$NUM_TEACH<-NULL

#make observations with STUD_TEACH_RATIO=="Inf" in prof_data NA
prof_data$STUD_TEACH_RATIO[which(prof_data$STUD_TEACH_RATIO=="Inf")]<-NA
prof_data$STUD_TEACH_RATIO[which(is.na(prof_data$STUD_TEACH_RATIO))]<-mean(prof_data$STUD_TEACH_RATIO, na.rm = T)



##########################################################################
#  TASK 8: MERGE prof_data with educational attainment in district  data #
##########################################################################
#load data
dist_ed<-read.csv("ACS_16_5YR_S1501_with_ann_ed.csv", colClasses = "character", na.strings = c("","s"))


#keep relevant columns in dist_ed object
dist_ed<-dist_ed[,c("Geography","Percent..Estimate..Population.18.to.24.years...High.school.graduate..includes.equivalency.")]

#change colnames in dist_ed object
names(dist_ed)[which(names(dist_ed)=="Geography")]<-"DISTRICT_NAME"
names(dist_ed)[which(names(dist_ed)=="Percent..Estimate..Population.18.to.24.years...High.school.graduate..includes.equivalency.")]<-"PER_HS_DEGREE"


#change DISTRICT_NAME in dist_ed obj from lower to upper case
dist_ed$DISTRICT_NAME<-toupper(dist_ed$DISTRICT_NAME)


#keep first two words in district name and remove punctuation
school_dist_names<-as.data.frame(cbind(word(dist_ed$DISTRICT_NAME,1,6,sep=" "), dist_ed$PER_HS_DEGREE))
colnames(school_dist_names)<-c("DISTRICT_NAME_1","PER_HS_DEGREE")
school_dist_names$DISTRICT_NAME_1<-gsub('[[:punct:] ]+',' ',school_dist_names$DISTRICT_NAME_1)
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
school_dist_names$DISTRICT_NAME_1 <- trim(school_dist_names$DISTRICT_NAME_1)


#make column with first two words in district name and remove punctuation
prof_data$DISTRICT_NAME_1<-word(prof_data$DISTRICT_NAME,1,6,sep=" ")
prof_data$DISTRICT_NAME_1<-gsub('[[:punct:] ]+',' ',prof_data$DISTRICT_NAME_1)


#merge prof_data and school_dist_names and by DISTRICT_NAME_1
prof_data<-merge(prof_data,school_dist_names, by="DISTRICT_NAME_1", all = T)
prof_data<-prof_data[!is.na(prof_data$ENTITY_CD),]

prof_data$PER_HS_DEGREE<-as.numeric(as.character(prof_data$PER_HS_DEGREE))

#complete the remaining cases
prof_data<-complete_col(prof_data,"PER_HS_DEGREE", 100)






##########################################################################
#  TASK 9: MERGE prof_data with Economic attainment in district  data    #
##########################################################################
#load data Mean income (dollars); Estimate; All households - With earnings
dist_econ<-read.csv("ACS_16_5YR_S1902_with_ann.csv", colClasses = "character", na.strings = c("","s","N"))


#keep relevant columns
dist_econ<-dist_econ[,c("Geography","Mean.income..dollars...Estimate..All.households")]

#change colnames in dist_econ objects
names(dist_econ)[which(names(dist_econ)=="Geography")]<-"DISTRICT_NAME"
names(dist_econ)[which(names(dist_econ)=="Mean.income..dollars...Estimate..All.households")]<-"MEAN_INC"


#change DISTRICT_NAME column in dist_econ object from lower to upper case
dist_econ$DISTRICT_NAME<-toupper(dist_econ$DISTRICT_NAME)


#keep first two words in district name and remove punctuation
school_dist_names<-as.data.frame(cbind(word(dist_econ$DISTRICT_NAME,1,6,sep=" "), dist_econ$MEAN_INC))
colnames(school_dist_names)<-c("DISTRICT_NAME_1","MEAN_INC")
school_dist_names$DISTRICT_NAME_1<-gsub('[[:punct:] ]+',' ',school_dist_names$DISTRICT_NAME_1)
school_dist_names$DISTRICT_NAME_1 <- trim(school_dist_names$DISTRICT_NAME_1)


#merge prof_data and school_dist_names and by DISTRICT_NAME_1
prof_data<-merge(prof_data, school_dist_names, by="DISTRICT_NAME_1", all = T)
prof_data<-prof_data[!is.na(prof_data$ENTITY_CD),]


#complete the remaining cases
prof_data$MEAN_INC<-as.numeric(as.character(prof_data$MEAN_INC))
prof_data<-complete_col(prof_data,"MEAN_INC", Inf)



#####################################################
# TASK 10: MERGE prof_data with school financial    #
#####################################################
#load data
financial<-read.delim("Sdf14_1a.txt",  na.strings = c("","s","-1", "-2"))

#keep relevant variables in financial object
financial_1<-financial[,c("NAME", "STABBR", "TOTALEXP")]

#subset financial_1 object to keep observations from NY
financial_1<-subset(financial_1, STABBR=="NY")

#drop STABBR variable in financial_1 object
financial_1$STABBR<-NULL

#keep first two words of school district
financial_1$DISTRICT_NAME_1<-word(as.character(financial_1$NAME),1,3,sep=" ")
financial_1$DISTRICT_NAME_1<-trim(gsub('[[:punct:] ]+',' ',financial_1$DISTRICT_NAME_1))
financial_1$NAME<-NULL

#make column with first two words in district name and remove punctuation
prof_data$DISTRICT_NAME_1<-word(prof_data$DISTRICT_NAME,1,3,sep=" ")
prof_data$DISTRICT_NAME_1<-trim(gsub('[[:punct:] ]+',' ',prof_data$DISTRICT_NAME_1))


#merge prof_data and school_dist_names and by DISTRICT_NAME_1
prof_data<-merge(prof_data, financial_1, by="DISTRICT_NAME_1", all = T)
prof_data<-prof_data[!is.na(prof_data$ENTITY_CD),]

id_list<-unique(prof_data$ENTITY_CD)
new_data<-list()


for (id in id_list){
  
  #subet for each school in list
  id_data<-prof_data[which(prof_data$ENTITY_CD==id),]
  number_na=0
  
  for (row in 1:nrow(id_data)){
    
  # if TOTALEXP for row in id_data is not NA
    if (!is.na(id_data$TOTALEXP[row])){
      
      #rbind the respective data to a new dataset
      new_data<-rbind(new_data, id_data[row,])
      break
    }
    
    number_na=number_na+1
    
    
    if(number_na==nrow(id_data)){
      new_data<-rbind(new_data, id_data[row,])
    }
    
  }
}



prof_data<-as.data.frame(new_data)

#complete the remaining cases
prof_data$TOTALEXP<-as.numeric(as.character(prof_data$TOTALEXP))
prof_data<-complete_col(prof_data,"TOTALEXP", Inf)


#Expenditure per student
prof_data$EXP_PER_ST<-prof_data$TOTALEXP/prof_data$MEAN_ENROLL
prof_data$TOTALEXP<-NULL



##################################################
# TASK 11: Merge prof_data with teacher scores
#################################################
#load data
teach<-read.csv("APPR_RESEARCHER_DATA_PART_C_ORIGINAL_SCHOOL_2016.csv", colClasses = "character", na.strings = c("","s","suppressed"))


#change column overall_score in teach object from character to numeric
teach$OVERALL_SCORE<-as.numeric(teach$OVERALL_SCORE)

#for each school average over years
teach<-
  teach%>%
  group_by(SCHOOL_BEDS)%>%
  summarise(MEAN_TEACH_SCORE=mean(OVERALL_SCORE, na.rm = T))

#add column names to the object teach
colnames(teach)<-c("ENTITY_CD", "MEAN_TEACH_SCORE")


#merge object teach and objet prof_Data by entity and keep all rows
prof_data<-merge(prof_data, teach, by="ENTITY_CD", all=T)

#keep observations of prof_data that thave entity_CD and entity_name
prof_data<-prof_data[!is.na(prof_data$ENTITY_CD),]
prof_data<-prof_data[!is.na(prof_data$ENTITY_NAME),]

#complete column MEAN_TEACH_SCORE in the object prof_data
prof_data<-complete_col(prof_data,"MEAN_TEACH_SCORE", 100)


#################################################
# TASK 12: Create Charter School Indicator
#################################################

#create indicator for charter school using the 7th and 8th digit of school codes 
prof_data$CHARTER<-ifelse(substr(prof_data$ENTITY_CD, start=7, stop=8)=="86",1,0)



#################################################
# TASK 13: Merge with crime data
#################################################
#load data
crime<-read.csv("County_total.csv", colClasses = "character", na.strings = c("","s","suppressed"))

#remove commas from column Violent and Property in object crime
#change class of column from character to numeric
crime$Violent<-gsub(",","",crime$Violent)
crime$Violent<-as.numeric(crime$Violent)

crime$Property<-gsub(",","",crime$Property)
crime$Property<-as.numeric(crime$Property)

#for each county violent crimes average over years
violent<-
  crime%>%
  group_by(County)%>%
  summarise(mean_violent=mean(Violent, na.rm = T))

#for each county property crimes average over years
property<-
  crime%>%
  group_by(County)%>%
  summarise(mean_property=mean(Property, na.rm = T))

#merge violent and property objects by county data
crime<-merge(violent,property, by="County")

#load population data
county_pop<-read.csv("ACS_16_5YR_B01003_with_ann.csv", colClasses = "character", na.strings = c("","s","suppressed"))

#change column name in county_pop object to county
names(county_pop)[names(county_pop) == 'Geography'] <- 'County'

#change class of Estimate..Total from character to numeric
county_pop$Estimate..Total<-as.numeric(county_pop$Estimate..Total)

#extract first word in county_pop object
county_pop$County<-word(county_pop$County,1)

#merge crime and county_pop object by county
crime<-merge(crime,county_pop, by="County")

#create crime by citizen columns
crime$VIOL_CRIME_PER_CITIZEN<-crime$mean_violent/crime$Estimate..Total
crime$PROP_CRIME_PER_CITIZEN<-crime$mean_property/crime$Estimate..Total

#drop columns mean_violent,mean_property,Estimate..Total
crime$mean_violent<-NULL
crime$mean_property<-NULL
crime$Estimate..Total<-NULL

#change column name of crime object
names(crime)[names(crime) == 'County'] <- 'COUNTY_NAME'

#change column from COUNTY_NAME to upper case
crime$COUNTY_NAME<-toupper(crime$COUNTY_NAME)

#merge prof_data and crime object by COUNTY_NAME
prof_data<-merge(prof_data, crime, by="COUNTY_NAME")


#################################################################
# TASK 14: Merge with total population by school district data
#################################################################
#load school district data
school_pop<-read.csv("ACS_16_5YR_S0101_with_ann.csv", colClasses = "character", na.strings = c("","s","suppressed"))

#keep first two words of school district
school_pop$DISTRICT_NAME_1<-trim(gsub('[[:punct:] ]+',' ',school_pop$Geography))
school_pop$DISTRICT_NAME_1<-word(as.character(school_pop$DISTRICT_NAME_1),1,3,sep=" ")
school_pop$Geography<-NULL
school_pop$DISTRICT_NAME_1<-toupper(school_pop$DISTRICT_NAME_1)
names(school_pop)[which(names(school_pop)=="Total..Estimate..Total.population")]<-"TOTAL_DIST_POP"

#make column with first two words in district name and remove punctuation
prof_data$DISTRICT_NAME_1<-trim(gsub('[[:punct:] ]+',' ',prof_data$DISTRICT_NAME))
prof_data$DISTRICT_NAME_1<-word(prof_data$DISTRICT_NAME_1,1,3,sep=" ")

#merge prof_data and school_dist_names and by DISTRICT_NAME_1
prof_data<-merge(prof_data, school_pop, by="DISTRICT_NAME_1", all = T)
prof_data<-prof_data[!is.na(prof_data$ENTITY_CD),]

#extract  unique school ids
id_list<-unique(prof_data$ENTITY_CD)
new_data<-list()

#for each id in id_list
for (id in id_list){
  
  #subset data by school id
  id_data<-prof_data[which(prof_data$ENTITY_CD==id),]
  number_na=0
  
  for (row in 1:nrow(id_data)){
    
    #if not TOTAL_DIST_POP is NA
    if (!is.na(id_data$TOTAL_DIST_POP[row])){
      new_data<-rbind(new_data, id_data[row,])
      break
    }
    
    number_na=number_na+1
    
    if(number_na==nrow(id_data)){
      new_data<-rbind(new_data, id_data[row,])
    }
    
  }
}


#change class of new data to data.frame
prof_data<-as.data.frame(new_data)

#change class of TOTAL_DIST_POP from character to numeric
prof_data$TOTAL_DIST_POP<-as.numeric(as.character(prof_data$TOTAL_DIST_POP))

#complete the remaining cases
prof_data<-complete_col(prof_data,"TOTAL_DIST_POP", Inf)



##############################################
# TASK 12: write-out csv file                #
##############################################
#drop DISTRICT_NAME_1 DISTRICT_CD COUNTY_CD
prof_data$DISTRICT_NAME_1<-NULL
prof_data$DISTRICT_CD<-NULL
prof_data$COUNTY_CD <-NULL


#round all numeric variables to 4 decimal places
prof_data[,6:26]<-round(prof_data[,6:26],3)

#Using 80 percent of data, make imbalanced training dataset retaining proportions
classes = lapply(levels(as.factor(prof_data$PROF_LEVEL)), function(x) which(prof_data$PROF_LEVEL==x))
set.seed(1)
train = lapply(classes, function(class) sample(class, 0.8*length(class), replace = F))
train = unlist(train)
imbalanced_train = prof_data[train,] #imbalanced training set
test = prof_data[-train,] #test set


#balanced dataset undersample majority
set.seed(1)
bal_prof<-prof_data[sample(which(prof_data$PROF_LEVEL=="Proficient"),259, replace=FALSE),]

set.seed(1)
bal_not_prof<-prof_data[sample(which(prof_data$PROF_LEVEL=="Not Proficient"),259, replace=FALSE),]

#rbind the subsets
balanced_under_sample<-rbind(bal_prof, bal_not_prof)


#balanced dataset oversample minority
bal_prof<-prof_data[sample(which(prof_data$PROF_LEVEL=="Proficient"),2620, replace=TRUE),]
bal_not_prof<-prof_data[sample(which(prof_data$PROF_LEVEL=="Not Proficient"),2620, replace=FALSE),]

balanced_over_sample<-rbind(bal_prof, bal_not_prof)

#write out datasets
write.csv(prof_data,"proficiency_data.csv", row.names = F)
write.csv(imbalanced_train,"imbalanced_train.csv", row.names = F)
write.csv(balanced_under_sample,"balanced_under_train.csv", row.names = F)
write.csv(balanced_over_sample,"balanced_over_train.csv", row.names = F)
write.csv(test,"test.csv", row.names = F)





