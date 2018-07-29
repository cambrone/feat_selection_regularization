###########################################################################################
# TASK: Analyze Proficiency Data
# Author: Andres Cambronero
# Project: Feature Selection via Regulatization: School Proficiency Case
# Date Started: June 7, 2018
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
library(tidyr)
library(broom)
library(glmnet)
library(plotmo)
library(caret)
library(bootstrap)

#set working directory
setwd("~/Desktop/summer_projects/NY_school_performance/data")

#load imbalanced training and test data
train<-read.csv("imbalanced_train.csv", colClasses = "character")
test<-read.csv("test.csv", colClasses = "character")

#change class of columns 6:30 from character to numeric in training and test data
train[,6:30]<-sapply(train[,6:30], as.numeric)
test[,6:30]<-sapply(test[,6:30], as.numeric)

#change class of charter column to factor for training and test data
train$CHARTER<-as.factor(train$CHARTER)
test$CHARTER<-as.factor(test$CHARTER)

#change class of PROF_LEVEL to factor in training and test data
train$PROF_LEVEL<-as.factor(train$PROF_LEVEL)
test$PROF_LEVEL<-as.factor(test$PROF_LEVEL)

#drop ENTITY_CD ENTITY_NAME DISTRICT_NAME COUNTY_NAME in training and test data
train$ENTITY_CD<-NULL
train$ENTITY_NAME <-NULL    
train$DISTRICT_NAME<-NULL
train$COUNTY_NAME<-NULL
test$ENTITY_CD<-NULL
test$ENTITY_NAME <-NULL    
test$DISTRICT_NAME<-NULL
test$COUNTY_NAME<-NULL


########################################################
# LOGISTIC REGRESSION with all variables as predictors
########################################################
#scale predictors in traning data 
train[,2:22]<-scale(train[,2:22])
train[,24:26]<-scale(train[,24:26])

#scale predictors in test data
test[,2:22]<-scale(test[,2:22])
test[,24:26]<-scale(test[,24:26])

#run logistic regression using all variables as predictors
logistic_all <- glm(PROF_LEVEL ~ . , data = train, family = "binomial")

# CHECK ASSUMPTTIONS OF LOGISTIC REGRESSION 
# Lineartiy: perceived non-linearity due to  outliers
probabilities <- predict(logistic_all, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "Proficient", "Not Proficient")

#add names to predictors
predictors<-names(train)[which(names(train)!="PROF_LEVEL")]

#plot predicted probabilities
linearity<-
  train[,2:22] %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(linearity, aes(logit, predictor.value))+
  geom_point(size = 0.2, alpha = 0.2) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")


#Influential points: A few influential points
plot(logistic_all, which = 4, id.n = 3)

model.data <- 
  augment(logistic_all) %>% 
  mutate(index = 1:n()) 

model.data%>% 
  top_n(3, .cooksd)

#Outliers: A few infuential points
ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = PROF_LEVEL), alpha = .5, size=.5) +
  theme_bw()

outliers<-
model.data %>% 
  filter(abs(.std.resid) > 3)

# Results of Logistic model using data without outliers. 
no_out_train<-train[-(outliers$index),]
no_out_logistic_all <- glm(PROF_LEVEL ~ . , data = no_out_train, family = "binomial")
summary(no_out_logistic_all)

#calculate Pseudo-Rsquared
1-(logistic_all$deviance/logistic_all$null.deviance)

#Prediction Accuracy logistic all data points v  data without outliers
predicted.classes <- ifelse(predict(logistic_all, test) > 0.5, "Proficient", "Not Proficient")
predicted.classes_no_out <- ifelse(predict(no_out_logistic_all, test) > 0.5, "Proficient", "Not Proficient")

table(predicted.classes, test$PROF_LEVEL)
table(predicted.classes_no_out, test$PROF_LEVEL)

# Results Logistic scaled 
summary(logistic_all)
confint(logistic_all)
confint.default(logistic_all)
exp(cbind(OR = coef(logistic_all), confint(logistic_all)))
with(logistic_all, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))


#Pseudo-Rsquared
1-(logistic_all$deviance/logistic_all$null.deviance)

#Prediction Accuracy logistic all data points v  data without outliers
predicted.classes <- ifelse(predict(logistic_all, test) > 0.5, "Proficient", "Not Proficient")
table(predicted.classes, test$PROF_LEVEL)


##############################################################
#LOGISTIC SCALED with selected variables as predictors
##############################################################
#run logistic regression with selected variables
logistic_selected <- glm(PROF_LEVEL ~ PER_BLACK + PER_HISP+ PER_ASIAN + PER_WHITE +
                      PER_ECDIS + PER_SUSPENSIONS+ MEAN_INC+ EXP_PER_ST+ 
                      PROP_CRIME_PER_CITIZEN+ TOTAL_DIST_POP + CHARTER, 
                    data = train, family = "binomial")

#summary of results
summary(logistic_selected)

#calculate confidence intervals
confint(logistic_selected)

#test whether model is better than intercept model alone
with(logistic_selected, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))


#Pseudo-Rsquared
1-(logistic_selected$deviance/logistic_selected$null.deviance)


#Prediction Accuracy logistic all data points v  data without outliers
predicted.classes <- ifelse(predict(logistic_selected, test) > 0.5, "Proficient", "Not Proficient")
table(predicted.classes, test$PROF_LEVEL)

#BOOTSTRAP prediction accuracy
n<-nrow(train)
B<-1000

boot_accuracy<-list()

for (b in 1:B){
  #sample ids
  i=sample(1:n, size=n, replace=T)
  
  #extract ids
  boot_samp<-train[i,]
  
  #run model 
  boot_logistic_selected <- glm(PROF_LEVEL ~ PER_BLACK + PER_HISP+ PER_ASIAN + PER_WHITE +
                             PER_ECDIS + PER_SUSPENSIONS+ MEAN_INC+ EXP_PER_ST+ 
                             PROP_CRIME_PER_CITIZEN+ TOTAL_DIST_POP + CHARTER, 
                           data = boot_samp, family = "binomial")  

  
  #make prediction using above model
  predicted.classes<-ifelse(predict(boot_logistic_selected, test) > 0.5, "Proficient", "Not Proficient")
  
  #create confusion matrix
  confusion_matrix<-table(predicted.classes, test$PROF_LEVEL)
  
  #extract overall accuracy and place in list
  boot_accuracy[b]<-sum(diag(confusion_matrix))/sum(confusion_matrix)
}


# 95CI% on Prediction Accuracy
boot_accuracy<-unlist(boot_accuracy)
boot_selected_CI_acc<-quantile(boot_accuracy, c(0.025, 0.975))

#write out booot trap confidence intervals
write.csv(boot_selected_CI_acc, "~/Desktop/summer_projects/NY_school_performance/data/boot_selected_CI_acc.csv", row.names = F)



##############################
# RIDGE REGRESSION 
###############################
# separate predictors and response variable
predictors<-data.matrix(train[,2:26])
proficiency_level<-as.factor(train[,1])

#run model 
ridge_model <- glmnet(predictors,proficiency_level, family="binomial", alpha=0)

#plot of deviations (variation explained)
plot_glmnet(ridge_model, label=10, "dev")

#plot of non-zero coeff at different lambdas
plot_glmnet(ridge_model, label=10)


#lambda cross-validation to select ideal 
set.seed(100)
ridge_cv_std <- cv.glmnet(predictors, proficiency_level,
                          type.measure="class", nfolds=20,
                          family="binomial", alpha=0)


#plot of CV misclassification error min and within 1 standard deviation
plot(ridge_cv_std)


#coefficients at min lambda and within one standard deviation 
coef(ridge_cv_std, s = "lambda.min")
coef(ridge_cv_std, s= "lambda.1se")


#test prediction accuracy at lowest lambda and at within one standrd deviation
test_x<-data.matrix(test[,2:26])
ridge_prediction_min<-predict(ridge_cv_std, newx = test_x, s = "lambda.min", type = "class")
ridge_prediction_1se<-predict(ridge_cv_std, newx = test_x, s = "lambda.1se", type = "class")

#confusion matrices
table(ridge_prediction_min, test$PROF_LEVEL)
table(ridge_prediction_1se, test$PROF_LEVEL)

#Ridge bootstrap using lambda min from cross validation 
n<-nrow(train)
B<-1000
lambda_1se<-ridge_cv_std$lambda.1se

boot_coef_lambda<-list()
boot_accuracy<-list()

for (b in 1:B){
  #sample ids
  i=sample(1:n, size=n, replace=T)
  
  #extract ids
  boot_samp<-train[i,]
  
  #set up predictors and response according to GLMNET
  predictors<-data.matrix(boot_samp[,2:26])
  proficiency_level<-as.factor(boot_samp[,1])
  
  #run model 
  boot_ridge_model <- glmnet(predictors,proficiency_level, family="binomial", alpha=0)
  
  #extract coefficients
  boot_coef_lambda[b]<-list(coef(boot_ridge_model, s=lambda_1se))
  
  #make prediction using above model
  prediction<-predict(boot_ridge_model, newx = test_x, s = lambda_1se, type = "class")

  #create confusion matrix
  confusion_matrix<-table(test$PROF_LEVEL, prediction)
  
  #extract overall accuracy and place in list
  boot_accuracy[b]<-sum(diag(confusion_matrix))/sum(confusion_matrix)
}

#column bind the results
boot_coef_lambda<-do.call(cbind, boot_coef_lambda)
boot_accuracy<-unlist(boot_accuracy)


# 95CI% on each Predictor Ridge Regression 
boot_ridge_CI_coef<-apply(boot_coef_lambda, 1, quantile, probs = c(0.025, 0.975))

#write out bootstrap confidence intervals on coefficients
write.csv(boot_ridge_CI_coef, "~/Desktop/summer_projects/NY_school_performance/data/boot_ridge_CI_coef.csv", row.names = F)


# 95CI% on Prediction Accuracy
boot_ridge_CI_acc<-quantile(boot_accuracy, c(0.025, 0.975))

#write out bootsp confidence intervals on test accuracy
write.csv(boot_ridge_CI_acc, "~/Desktop/summer_projects/NY_school_performance/data/boot_ridge_CI_acc.csv", row.names = F)



#####################################
# ELASTIC NET REGRESSION 
#####################################
# separate predictors and response variable
predictors<-data.matrix(train[,2:26])
proficiency_level<-as.factor(train[,1])

#create list of possible value of alpha to choose from
alpha_list<-seq(from=0.05, to=0.95, by=0.05)

cv_elastic_min<-c()
cv_elastic_1se<-c()

for (i in 1:length(alpha_list)){
  set.seed(100)
  #run elastic net using each value in alpha_list
  elastic_cv <- cv.glmnet(predictors, proficiency_level,
                            type.measure="class", nfolds=20,
                            family="binomial", alpha=alpha_list[i])
 
  #extract the lambda.min and lambda.1se for each loop
  cv_elastic_min[i] <- elastic_cv$lambda.min
  cv_elastic_1se[i] <- elastic_cv$lambda.1se
}

#select alpha is the one used that produced mininal error
cv_alpha<-alpha_list[which(cv_elastic_min==min(cv_elastic_min))]

#write out cv_alpha
write.csv(cv_alpha, "~/Desktop/summer_projects/NY_school_performance/data/cv_alpha.csv", row.names = F)

#using cv_alpha run elastic net regression 
elastic_model <- glmnet(predictors, proficiency_level, family="binomial", alpha=cv_alpha)

#plot of non-zero coeff at different lambdas
plot_glmnet(elastic_model, label=10)

#plot of deviations (variation explained)
plot_glmnet(elastic_model, label=10, "dev")

#lambda cross-validation
set.seed(100)
elastic_cv_std <- cv.glmnet(predictors, proficiency_level, 
                          type.measure="class", nfolds=20,
                          family="binomial", alpha=cv_alpha)

#plot of CV misclassification error min and within 1 standard deviation
plot(elastic_cv_std)


#coefficients at min lambda and within one standard deviation 
coef(elastic_cv_std, s = "lambda.min")
coef(elastic_cv_std, s= "lambda.1se")


#test prediction accuracy at lowest lambda and at within one standrd deviation
test_x<-data.matrix(test[,2:26])
elastic_prediction_min<-predict(elastic_cv_std, newx = test_x, s = "lambda.min", type = "class")
elastic_prediction_1se<-predict(elastic_cv_std, newx = test_x, s = "lambda.1se", type = "class")


#confusion matrices
table(elastic_prediction_min, test$PROF_LEVEL)
table(elastic_prediction_1se, test$PROF_LEVEL)


#Elastic bootstrap using lambda min and alpha from cross validation 
n<-nrow(train)
B<-1000
lambda_1se<-elastic_cv_std$lambda.1se

boot_coef_lambda<-list()
boot_accuracy<-list()

for (b in 1:B){
  #sample ids
  i=sample(1:n, size=n, replace=T)
  
  #extract ids
  boot_samp<-train[i,]
  
  #set up predictors and response according to GLMNET
  predictors<-data.matrix(boot_samp[,2:26])
  proficiency_level<-as.factor(boot_samp[,1])
  
  #run model 
  boot_elastic_model <- glmnet(predictors, proficiency_level, family="binomial", alpha=cv_alpha)
  
  #extract coefficients
  boot_coef_lambda[b]<-list(coef(boot_elastic_model, s=lambda_1se))
  
  #make prediction using above model
  prediction<-predict(boot_elastic_model, s = lambda_1se, newx = test_x, type = "class")
  
  #create confusion matrix
  confusion_matrix<-table(test$PROF_LEVEL, prediction)
  
  #extract overall accuracy and place in list
  boot_accuracy[b]<-sum(diag(confusion_matrix))/sum(confusion_matrix)
}


# Percent of samples in which each predictor was used
boot_coef_lambda<-do.call(cbind, boot_coef_lambda)
boot_coef_lambda<-as.data.frame(as.matrix(boot_coef_lambda))
boot_coef_lambda[boot_coef_lambda == 0] <- NA

boot_elastic_usage<-apply(boot_coef_lambda, 1, function(x) sum(!is.na(x))/1000)
boot_elastic_usage<-as.data.frame(boot_elastic_usage)

#write out boot_elastic_usage
write.csv(boot_elastic_usage, "~/Desktop/summer_projects/NY_school_performance/data/boot_elastic_usage.csv")


# 95CI% on each Predictor elastic Regression 
boot_elastic_CI_coef<-apply(boot_coef_lambda, 1, quantile, probs = c(0.025, 0.975),  na.rm = T)

#write out boot_elastic_CI_coef
write.csv(boot_elastic_CI_coef, "~/Desktop/summer_projects/NY_school_performance/data/boot_elastic_CI_coef.csv", row.names = F)


# 95CI% on Prediction Accuracy
boot_accuracy<-unlist(boot_accuracy)
boot_elastic_CI_acc<-quantile(boot_accuracy, c(0.025, 0.975),  na.rm = T)

#write out boot_elastic_CI_acc
write.csv(boot_elastic_CI_acc, "~/Desktop/summer_projects/NY_school_performance/data/boot_elastic_CI_acc.csv", row.names = F)


##############################
# LASSO Regression 
##############################
# separate predictors and response variable
predictors<-data.matrix(train[,2:26])
proficiency_level<-as.factor(train[,1])

#run LASSO  
lasso_model <- glmnet(predictors, proficiency_level, family="binomial", alpha=1)

#plot of non-zero coeff at different lambdas
plot_glmnet(lasso_model, label=10)

#plot of deviations (variation explained)
plot_glmnet(lasso_model, label=10, "dev")

#lambda cross-validation
set.seed(100)
lasso_cv_std <- cv.glmnet(predictors, proficiency_level, 
                          type.measure="class", nfolds=20,
                          family="binomial", alpha=1)


#plot of CV misclassification error min and within 1 standard deviation
plot(lasso_cv_std)


#coefficients at min lambda and within one standard deviation 
coef(lasso_cv_std, s = "lambda.min")
coef(lasso_cv_std, s= "lambda.1se")


#test prediction accuracy at lowest lambda and at within one standrd deviation
lasso_prediction_min<-predict(lasso_cv_std, newx = test_x, s = "lambda.min", type = "class")
lasso_prediction_1se<-predict(lasso_cv_std, newx = test_x, s = "lambda.1se", type = "class")


#confusion matrices
table(lasso_prediction_min, test$PROF_LEVEL)
table(lasso_prediction_1se, test$PROF_LEVEL)



#lasso bootstrap using lambda min and alpha from cross validation 
n<-nrow(train)
B<-1000
lambda_1se<-lasso_cv_std$lambda.1se

boot_coef_lambda<-list()
boot_accuracy<-list()

for (b in 1:B){
  #sample ids
  i=sample(1:n, size=n, replace=T)
  
  #extract ids
  boot_samp<-train[i,]
  
  #set up predictors and response according to GLMNET
  predictors<-data.matrix(boot_samp[,2:26])
  proficiency_level<-as.factor(boot_samp[,1])
  
  #run model 
  boot_lasso_model <- glmnet(predictors, proficiency_level, family="binomial", alpha=1)
  
  #extract coefficients
  boot_coef_lambda[b]<-list(coef(boot_lasso_model, s=lambda_1se))
  
  #make prediction using above model
  prediction<-predict(boot_lasso_model, newx = test_x, s = lambda_1se, type = "class")
  
  #create confusion matrix
  confusion_matrix<-table(test$PROF_LEVEL, prediction)
  
  #extract overall accuracy and place in list
  boot_accuracy[b]<-sum(diag(confusion_matrix))/sum(confusion_matrix)
}

#column bind the coefficients
boot_coef_lambda<-do.call(cbind, boot_coef_lambda)
boot_coef_lambda[boot_coef_lambda == 0] <- NA

#percent of times each variable was used
boot_lasso_usage<-apply(boot_coef_lambda, 1, function(x) sum(!is.na(x))/1000)
boot_lasso_usage<-as.data.frame(boot_lasso_usage)

#write out boot_lasso_usage
write.csv(boot_lasso_usage, "~/Desktop/summer_projects/NY_school_performance/data/boot_lasso_usage.csv")


# 95CI% on each Predictor elastic Regression 
boot_lasso_CI_coef<-apply(boot_coef_lambda, 1, quantile, probs = c(0.025, 0.975),  na.rm = T)

#write out boot_lasso_CI_coef
write.csv(boot_lasso_CI_coef, "~/Desktop/summer_projects/NY_school_performance/data/boot_lasso_CI_coef.csv", row.names = F)


# 95CI% on Prediction Accuracy
boot_accuracy<-unlist(boot_accuracy)
boot_lasso_CI_acc<-quantile(boot_accuracy, c(0.025, 0.975))

#write out boot_lasso_CI_acc
write.csv(boot_lasso_CI_acc, "~/Desktop/summer_projects/NY_school_performance/data/boot_lasso_CI_acc.csv", row.names = F)

