#Title : 707 Project - Gradient Boosting Algorithm
#@Author : Sanjana Rajagopala
#Start Date : November 19, 2018

#Load the required libraries
library(caret)
library(dplyr)
library(arules)
library(klaR)
library(tictoc)
library(mlbench)
library(pROC)
library(gbm)
library(ROSE)
library(rpart)
################################
#
#Introduction to Gradient Boosting classifier
# The Gradient boosting classifier is an ensemble of weak prediction models such as decision tree. It builds stage-wise models
# and generalizes each of them using optimization of loss function.
###############################

#Step 1: Data preprocessing for Gradient Boosting

##Retaining the input features to be categorical.Hence, all the columns in the
#dataset must be converted into categorical type.
GBM_data <- employee_data_copy

#Retain the columns that were discretized and remove the corresponding base columns
GBM_data$Age <- NULL
GBM_data$HourlyRate <- NULL
GBM_data$DistanceFromHome <- NULL
GBM_data$PercentSalaryHike <- NULL
GBM_data$YearsWithCurrManager <- NULL

dim(GBM_data)

#Investigate the numerical columns
all_indices <- sapply(GBM_data, is.numeric)

str(GBM_data[, all_indices])

#It can be observed that the columns - MonthlyIncome, MOnthlyRate contain significant and distinct numerical values. These cannot be 
#be directly converted into factors because it would result in unnecessarily large number of levels. Hence, we discretize
#them so that manageable number of columns are obtained.

GBM_data$MonthlyIncome <- discretize(GBM_data$MonthlyIncome, method = 'interval')
GBM_data$MonthlyRate <- discretize(GBM_data$MonthlyRate, method = 'interval', categories = 4)

#Now, convert the remaining numerical columns into categorical

GBM_data[, all_indices] <- lapply(GBM_data[,all_indices], as.factor)

str(GBM_data)


#Step 2: Creating data model

#Create Data parition with 80% as training and 20% as testing
#Use Cross validation on the training set while 
train_indices <- createDataPartition(GBM_data$Attrition 
                                     ,p = 0.8
                                     ,list = F)
gbm_train <- GBM_data[train_indices,]
gbm_test <- GBM_data[-train_indices,]

#Define the cross validation
tr_control <- trainControl(method = 'cv', number = 5, classProbs = T)

#Model 1 - Basic GBM Model
tic('GBM_Model_1')
GBM_model_1 = suppressWarnings(train(Attrition~., data=gbm_train
                                    , method="gbm"
                                    , verbose = F
                                    , metric="ROC"
                                    , trControl=tr_control))
toc()

#Predict using the GBM Model 1
predict_GBM1 <- suppressWarnings(
  predict(GBM_model_1
          ,newdata = gbm_test
          )
)

#Evaluating the model

confusionMatrix(as.factor(predict_RF1), as.factor(gbm_test$Attrition))
roc(as.numeric(gbm_test$Attrition), as.numeric(predict_GBM1))

#This model also shows a fairly poor AUC-ROC. Hence, it implies we need to handle class imbalance
#Class Imbalance problem
#In binary classification problem, when one class outnumbers the other class by a large proportion the machine learning
#algorithms do not get enough information to make accurate prediction for the minority class.  This is mainly 
#because the ML algorithms assume that the data set has balanced class distributions and errors obtained from different
#classes have same cost.

#This depicts the imbalance
table(GBM_data$Attrition)

#Model 2 -  Gradient Boosting Model with SMOTE


#SMOTE - It is the Synthetic Minority Oversampling Technique that generates artificial data based on feature space
# rather than data space similarities from minority samples. The bootstrapping and k-nearest neighbours techniques are used to achieve this.

tic('GBM_Model_2')

tr_control$sampling <-'smote'

GBM_model_2 = suppressWarnings(train(Attrition~., data=gbm_train
                   , method="gbm"
                   , metric="ROC"
                   , verbose = F
                   , trControl=tr_control))

toc()

#Predict using the smote-fit GBM Model 2
predict_gbm2 <- suppressWarnings(
  predict(GBM_model_2
          ,newdata = gbm_test
          )
)

#Evaluating the GBM model 2

confusionMatrix(as.factor(predict_gbm2), as.factor(gbm_test$Attrition))
auc(as.numeric(gbm_test$Attrition), as.numeric(predict_gbm2))

#Model 3 -  GBM Model using oversampled data

#Using the oversampling method in ROSE package
over_gbm_train <- ovun.sample(Attrition ~.,data = gbm_train, method = 'over')$data

table(over_gbm_train$Attrition)
#Now, it looks balanced

over_tr_control <- trainControl(method = 'cv', number = 5, classProbs = T)

tic('GBM_Model_3')

GBM_model_3 = suppressWarnings(train(Attrition~., data=over_gbm_train
                                    , method="gbm"
                                    , metric="ROC"
                                    , verbose = F
                                    , trControl=over_tr_control))

toc()


#Predict using the oversampled GBM  Model 3
predict_gbm3 <- suppressWarnings(
  predict(GBM_model_3
          ,newdata = gbm_test
          ))


#Evaluating the GBM model 3

confusionMatrix(as.factor(predict_gbm3), as.factor(gbm_test$Attrition))
auc(as.numeric(gbm_test$Attrition), as.numeric(predict_gbm3))

#An increase in the AUC from 63% to 73% by oversampling

#Model 4 -  GBM Model using under+over sampled data

#Using the under sampling method in ROSE package
under_gbm_train <- ovun.sample(Attrition ~.,data = gbm_train, method = 'under')$data

table(under_gbm_train$Attrition)
#Now, it looks balanced. However, there is significant loss of information as it can be observed that the number of obserrvations got reduced
#from ~980 to ~190. 


#Hence, choosing the combination of over and under sampling together.

both_gbm_train <- ovun.sample(Attrition~., data = gbm_train, method = 'both', p =0.5, seed = 1)$data

tic('GBM_Model_4')

GBM_model_4 = suppressWarnings(train(Attrition~., data=both_gbm_train
                                     , method="gbm"
                                     , metric="ROC"
                                     , verbose = F
                                     , trControl=over_tr_control))

toc()


#Predict using the over+under sampled GBM  Model 4
predict_gbm4 <- suppressWarnings(
  predict(GBM_model_4
          ,newdata = gbm_test
  ))


#Evaluating the GBM model 4

confusionMatrix(as.factor(predict_gbm4), as.factor(gbm_test$Attrition))
auc(as.numeric(gbm_test$Attrition), as.numeric(predict_gbm4))


#Model 5 -  GBM Model using the data sample by ROSE method

#Using the under sampling method in ROSE package
rose_gbm_train <- ROSE(Attrition ~.,data = gbm_train, seed = 1, N = 1470)$data

table(rose_gbm_train$Attrition)


tic('GBM_Model_5')

GBM_model_5 = suppressWarnings(train(Attrition~., data=rose_gbm_train
                                     , method="gbm"
                                     , metric="ROC"
                                     , verbose = F
                                     , trControl=over_tr_control))

toc()


#Predict using the rose sampled GBM  Model 5
predict_gbm5 <- suppressWarnings(
  predict(GBM_model_5
          ,newdata = gbm_test
  ))


#Evaluating the GBM model 5

confusionMatrix(as.factor(predict_gbm5), as.factor(gbm_test$Attrition))
auc(as.numeric(gbm_test$Attrition), as.numeric(predict_gbm5))





#Model 6 -  GBM Model using the weighting tecnhique in the dataset
#Weighting technique helps in dealing with class imbalance by punishing the errors in the minority class

set.seed(3000)

model_weights <- ifelse(gbm_train$Attrition == "No",
                        (1/table(gbm_train$Attrition)[1]) * 0.5,
                        (1/table(gbm_train$Attrition)[2]) * 0.5)


tic('GBM_Model_6')

GBM_model_6 = suppressWarnings(train(Attrition~., data=gbm_train
                                     , method="gbm"
                                     , metric="ROC"
                                     , verbose = F
                                     , trControl=over_tr_control
                                     , weights = model_weights))

toc()


#Predict using the rose sampled GBM  Model 6
predict_gbm6 <- suppressWarnings(
  predict(GBM_model_6
          ,newdata = gbm_test
  ))


#Evaluating the GBM model 6

confusionMatrix(as.factor(predict_gbm6), as.factor(gbm_test$Attrition))
gbm_auc <- auc(as.numeric(gbm_test$Attrition), as.numeric(predict_gbm6))
gbm_auc


#Plotting the accuracies of all the models


roc.curve(gbm_test$Attrition, predict_GBM1, col = 'black')
par(new=T)
roc.curve(gbm_test$Attrition, predict_gbm2, col = 'blue')
par(new=T)
roc.curve(gbm_test$Attrition, predict_gbm3, col = 'red')
par(new=T)
roc.curve(gbm_test$Attrition, predict_gbm4, col = 'green')
par(new=T)
roc.curve(gbm_test$Attrition, predict_gbm5, col = 'pink')
par(new=T)
roc.curve(gbm_test$Attrition, predict_gbm6, col = 'burlywood')

#It can be concluded that the GBM with oversampled dataset  and the GBM with weighting 
#gives the best AUC metric when compared to the other models. Both give almost the same value of ~74%.


