#Title : 707 Project - Support vector Machines Algorithm
#@Author : Sanjana Rajagopala
#Start Date : November 23, 2018


library(caret)
library(arules)
library(tictoc)
library(pROC)
library(e1071)
library(DMwR)

#Step 1: Data Preprocessing

train_svm_data <- employee_data


#The SVM model is capable of dealing with both numerical and categorical variables. The preprocessing
#steps remain same as that of KNN. Hence, we apply the standardization preprocessing.

#Apply standardization preprocessing on data during the modelling step


#Step 2: Building SVM models

#Create data partitions
train_svm_indices <- createDataPartition(train_svm_data$Attrition,
                                         p =0.8
                                         ,list=F
)
svm_train <- train_svm_data[train_svm_indices,]
svm_test <- train_svm_data[-train_svm_indices,]


svm_preprocess_1 <- suppressWarnings(preProcess(svm_train, method = c('scale', 'center')))
svm_preprocess_2 <- suppressWarnings(preProcess(svm_test, method = c('scale', 'center')))

svm_train <- predict(svm_preprocess_1, newdata = svm_train)
svm_test <- predict(svm_preprocess_2, newdata = svm_test)



tic('SVM_Model_1')
svm_model_1 <- suppressWarnings(train(
  Attrition~.,
  data = svm_train,
  method ='svmRadial',
  trControl = trainControl(method = 'cv', number = 5)
  #,tuneGrid = expand.grid(C = seq(1, 1.5), sigma = c(0.002,0.006))
))
toc()


predict_svm_1 <- predict(svm_model_1,newdata = svm_test)
confusionMatrix(predict_svm_1, svm_test$Attrition)

auc(as.numeric(svm_test$Attrition), as.numeric(predict_svm_1))



#SVM Model 2
tic('SVM_Model_2')
svm_model_2 <- suppressWarnings(train(
  Attrition~.,
  data = svm_train,
  method ='svmRadial',
  trControl = trainControl(method = 'cv', number = 5)
  ,tuneGrid = expand.grid(C = seq(1.5), sigma = c(0.0009, 0.006))
))
toc()
#expand the cost in SVM

predict_svm_2 <- predict(svm_model_2,newdata = svm_test)
confusionMatrix(predict_svm_2, svm_test$Attrition)

svm_auc <- auc(as.numeric(svm_test$Attrition), as.numeric(predict_svm_2))
svm_auc

#Irrespective of the number of changes to the tuning parameters, the AUC metric remains constant
#around the value of ~53%. This is a terrible score for a classifier - meaning it is as good 
#as a random classifier. Hence, not preferring SVM model. 


#Fine tuning the SVM Model

#SVM Model 3
tic('SVM_Model_3')
svm_model_3 <- suppressWarnings(train(
  Attrition~.,
  data = svm_train,
  method ='svmRadial',
  trControl = trainControl(method = 'cv', number = 5)
  ,tuneGrid = expand.grid(C = seq(0, 1, 0.1), sigma = c(0.0009, 0.006))
))
toc()
#expand the cost in SVM

predict_svm_3 <- predict(svm_model_3,newdata = svm_test)
confusionMatrix(predict_svm_3, svm_test$Attrition)

auc(as.numeric(svm_test$Attrition), as.numeric(predict_svm_3))


#First handle the class imbalnace and then apply SVM

table(svm_train$Attrition)

#Using SMOTE to create balanced dataset

#1. Get the attrition class count
attr_count <- table(svm_train$Attrition)

#2. Compute oversampling
o_count <- ((0.6 * max(attr_count)) - min(attr_count)) / min(attr_count)

#3. Compute under sampling
u_count <- (0.4 * max(attr_count)) / (min(attr_count) * o_count)

over_svm = round(o_count,1) * 100
under_svm = round(u_count, 1) * 100

svm_smote_data <-DMwR::SMOTE(Attrition ~.
                              ,svm_train
                              ,perc.over = over_svm
                              , k = 5
                              , perc.under = under_svm)

#Check the balanced dataset
table(svm_smote_data$Attrition)

#Removing the unnecessary columns
svm_smote_data$Age <- NULL
svm_smote_data$PercentSalaryHike <- NULL
svm_smote_data$HourlyRate <- NULL
svm_smote_data$DistanceFromHome <- NULL

tuned_svm = tune.svm(Attrition~., 
                     data = svm_smote_data
                     , cost=1:10,
                     gamma=seq(0,1,0.01) )

tuned_svm

#The best parameters are observed to be gamma = 0.04 and cost = 6. Hence, using these for the model
svm_model_4 <- svm(Attrition ~., data = svm_smote_data ,  kernel = "radial", gamma = 0.04, cost = 6)


predict_svm_4 <- predict(svm_model_4,newdata = svm_test)
confusionMatrix(predict_svm_4, svm_test$Attrition)

auc(as.numeric(svm_test$Attrition), as.numeric(predict_svm_4))


#Trying further

svm_model_5 <- svm(Attrition ~., data = svm_smote_data ,  kernel = "radial", gamma = 0.02, cost = 13)


predict_svm_5 <- predict(svm_model_5,newdata = svm_test)
confusionMatrix(predict_svm_4, svm_test$Attrition)

svm_auc <- auc(as.numeric(svm_test$Attrition), as.numeric(predict_svm_5))
svm_auc
