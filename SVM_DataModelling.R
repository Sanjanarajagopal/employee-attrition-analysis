#Title : 707 Project - Support vector Machines Algorithm
#@Author : Sanjana Rajagopala
#Start Date : November 23, 2018


library(caret)
library(arules)
library(tictoc)
library(pROC)

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


predict_svm_2 <- predict(svm_model_2,newdata = svm_test)
confusionMatrix(predict_svm_2, svm_test$Attrition)

svm_auc <- auc(as.numeric(svm_test$Attrition), as.numeric(predict_svm_2))
svm_auc

#Irrespective of the number of changes to the tuning parameters, the AUC metric remains constant
#around the value of ~53%. This is a terrible score for a classifier - meaning it is as good 
#as a random classifier. Hence, not preferring SVM model. 
