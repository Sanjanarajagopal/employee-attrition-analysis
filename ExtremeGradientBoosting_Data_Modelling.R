#Title : 707 Project - Extreme Gradient Boosting Algorithm
#@Author : Sanjana Rajagopala
#Start Date : November 20, 2018

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
library(DMwR)
library(xgboost)
################################
#
#Introduction to Extreme Gradient Boosting classifier
# The Extreme Gradient boosting classifier is similar to that of a gradient boosting classifier. However, it provides
#better performance because - 
#1. Formulization of more-regularized models to control overfitting
#2.Completes tasks at faster speed using parallel computation on a single machine
###############################


#The preprocessing steps remain same as that of the Gradient boosting algorithm. Hence, copying the datasets that were
#previously computed.

XGBM_Data <- GBM_data

#Consider the imbalance
table(XGBM_Data$Attrition)

#Using SMOTE to create balanced dataset

#1. Get the attrition class count
attrition_count <- table(XGBM_Data$Attrition)

#2. Compute oversampling
over_count <- ((0.6 * max(attrition_count)) - min(attrition_count)) / min(attrition_count)

#3. Compute under sampling
under_count <- (0.4 * max(attrition_count)) / (min(attrition_count) * over_count)

over = round(over_count,1) * 100
under = round(under_count, 1) * 100

XGBM_smote_data <-DMwR::SMOTE(Attrition ~.
                    ,XGBM_Data
                    ,perc.over = over
                    , k = 5
                    , perc.under = under)

#Check the balanced dataset
table(XGBM_smote_data$Attrition)

set.seed(100)


train_indices <- createDataPartition(XGBM_smote_data$Attrition 
                                     ,p = 0.8
                                     ,list = F)
xgbm_train <- XGBM_smote_data[train_indices,]
xgbm_test <- XGBM_smote_data[-train_indices,]

#Define the cross validation
tr_control <- trainControl(method = 'cv', number = 5, classProbs = T)
xgbm_grid <- expand.grid(
  max_depth = 20
  , subsample = 0.9
  , nrounds  = 200
  , gamma  = 0.01
  , colsample_bytree = 0.5
  , min_child_weight = 1
  , eta = 0.02
)

#Model 1 - Extreme GBM Model
tic('XGBM_Model')




XGBM_model = suppressWarnings(train(Attrition~., data=xgbm_train
                                     , method="xgbTree"
                                     , verbose = F
                                     , metric="ROC"
                                     , trControl=tr_control
                                    , tuneGrid = xgbm_grid))
toc()

#Predict using the XGBM Model 
predict_XGBM <- suppressWarnings(
  predict(XGBM_model
          ,newdata = xgbm_test
  )
)

#Evaluating the model

confusionMatrix(as.factor(predict_XGBM), as.factor(xgbm_test$Attrition))
xgbm_auc <- auc(as.numeric(xgbm_test$Attrition), as.numeric(predict_XGBM))
xgbm_auc

#Hence, this gives the best performance

