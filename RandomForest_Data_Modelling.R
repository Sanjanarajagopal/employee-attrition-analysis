#Title : 707 Project - Random Forest Algorithm
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

################################
#
#Introduction to Random Forest classifier
# The Random forest classifier is a supervised learning classifier with the base model as decision tree. The correlation
#between the trees is removed by picking random features at each split.
#
###############################

#Step 1: Data preprocessing for Random forest

##Retaining the input features to be categorical.Hence, all the columns in the
#dataset must be converted into categorical type.
RF_data <- employee_data_copy

#Retain the columns that were discretized and remove the corresponding base columns
RF_data$Age <- NULL
RF_data$HourlyRate <- NULL
RF_data$DistanceFromHome <- NULL
RF_data$PercentSalaryHike <- NULL
RF_data$YearsWithCurrManager <- NULL

dim(RF_data)

#Investigate the numerical columns
all_indices <- sapply(RF_data, is.numeric)

str(RF_data[, all_indices])

#It can be observed that the columns - MonthlyIncome, MOnthlyRate contain significant and distinct numerical values. These cannot be 
#be directly converted into factors because it would result in unnecessarily large number of levels. Hence, we discretize
#them so that manageable number of columns are obtained.

RF_data$MonthlyIncome <- discretize(RF_data$MonthlyIncome, method = 'interval')
RF_data$MonthlyRate <- discretize(RF_data$MonthlyRate, method = 'interval', categories = 4)

#Now, convert the remaining numerical columns into categorical

RF_data[, all_indices] <- lapply(RF_data[,all_indices], as.factor)

str(RF_data)


#Step 2: Creating data model

#Create Data parition with 80% as training and 20% as testing
#Use Cross validation on the training set while 
train_indices <- createDataPartition(RF_data$Attrition 
                                     ,p = 0.8
                                     ,list = F)
rf_train <- RF_data[train_indices,]
rf_test <- RF_data[-train_indices,]

#Define the cross validation
tr_control <- trainControl(method = 'cv', number = 5, classProbs = T)

#Model 1 - Basic Random Forest Model
tic('RF_Model_1')
RF_model_1 = suppressWarnings(train(Attrition~., data=rf_train
                                      , method="rf"
                                      , metric="ROC"
                                      , trControl=tr_control))

toc()

#Predict using the Logistic Regression Model 1
predict_RF1 <- suppressWarnings(
  predict(RF_model_1
          ,newdata = rf_test
          ,type = 'raw')
)

#Evaluating the model

confusionMatrix(as.factor(predict_RF1), as.factor(logR_test$Attrition))
auc(as.numeric(logR_test$Attrition), as.numeric(predict_RF1))


#Model 2 -  Random Forest Model
tic('RF_Model_2')
RF_model_2 = train(Attrition~., data=rf_train
                                    , method="rf"
                                    , metric="ROC"
                                    , tuneLength = 10
                                    , trControl=tr_control)

toc()

#Predict using the Random Forest Model 2
predict_RF2 <- suppressWarnings(
  predict(RF_model_2
          ,newdata = rf_test
          ,type = 'raw')
)

#Evaluating the RF model 2

confusionMatrix(as.factor(predict_RF2), as.factor(rf_test$Attrition))
auc(as.numeric(rf_test$Attrition), as.numeric(predict_RF2))

#Model 3 -  Random Forest Model
tic('RF_Model_3')
tunegrid <- expand.grid(.mtry=c(1:15))
RF_model_3 = suppressWarnings(train(Attrition~., data=rf_train
                                    , method="rf"
                                    , metric="ROC"
                                    , tuneLength = 10
                                    , tuneGrid = tunegrid
                                    , trControl=tr_control))

toc()

print(RF_model_3)

#Predict using the Random Forest Model 3
predict_RF3 <- suppressWarnings(
  predict(RF_model_3
          ,newdata = rf_test
          ,type = 'raw')
)


#Evaluating the RF model 3

confusionMatrix(as.factor(predict_RF3), as.factor(rf_test$Attrition))
auc(as.numeric(rf_test$Attrition), as.numeric(predict_RF3))

#Even after tuning the mtry and ntree parameters the performance is as bad as a classifier that randomly classifies the data.

#This is a very poor AUC and the model does not show some useful patterns





#Using the weighted sampling with Random forest

tic('RF_Model_4')
tunegrid <- expand.grid(.mtry=c(1:15))
RF_model_4 = suppressWarnings(train(Attrition~., data=rf_train
                                    , method="rf"
                                    , metric="ROC"
                                    , tuneLength = 10
                                    , tuneGrid = tunegrid
                                    , trControl=tr_control
                                    , weights = model_weights))

toc()


#Predict using the Random Forest Model 4
predict_RF4 <- suppressWarnings(
  predict(RF_model_4
          ,newdata = rf_test
  )
)


#Evaluating the RF model 4

confusionMatrix(as.factor(predict_RF4), as.factor(rf_test$Attrition))
rf_auc <- auc(as.numeric(rf_test$Attrition), as.numeric(predict_RF4))

rf_auc
#Does not show improvement in AUC when compared to that of GBM.



