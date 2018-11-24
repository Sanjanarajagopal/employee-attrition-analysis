#Title : 707 Project - Logistic Regression Algorithm
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
#Introduction to the Logistic Regression
# The Logistic regression is a supervised learning classifier best suited for binary classification problems. It uses concept of signmoid function
# and helps in the variable importance.
#
###############################

#Step 1: Data preprocessing for Logistic regression

##The logistic regression algorithm requires the input features to be categorical.Hence, all the columns in the
#dataset must be converted into categorical type.
LogR_data <- employee_data_copy

#Retain the columns that were discretized and remove the corresponding base columns
LogR_data$Age <- NULL
LogR_data$HourlyRate <- NULL
LogR_data$DistanceFromHome <- NULL
LogR_data$PercentSalaryHike <- NULL
LogR_data$YearsWithCurrManager <- NULL

dim(LogR_data)

#Investigate the numerical columns
all_indices <- sapply(LogR_data, is.numeric)

str(LogR_data[, all_indices])

#It can be observed that the columns - MonthlyIncome, MOnthlyRate contain significant and distinct numerical values. These cannot be 
#be directly converted into factors because it would result in unnecessarily large number of levels. Hence, we discretize
#them so that manageable number of columns are obtained.

LogR_data$MonthlyIncome <- discretize(LogR_data$MonthlyIncome, method = 'interval')
LogR_data$MonthlyRate <- discretize(LogR_data$MonthlyRate, method = 'interval', categories = 4)

#Now, convert the remaining numerical columns into categorical

LogR_data[, all_indices] <- lapply(LogR_data[,all_indices], as.factor)

#It can be obserbed that all the columns are converted into categorical type
str(LogR_data[,1:5])


#Additionally, when applying the train method = glm it is efficient to have only those columns with 2 or more levels
#Hence, eliminate the columns with less than 2 levels. This preprocessing is exclusive for Logistic regression method. 

#Define the function to check the number of levels
check_levels <- function(in_col)
{
  if(length(levels(in_col))>=2)
  {
    return(TRUE)
  }
  return(FALSE)
}

less_indices <- sapply(LogR_data, check_levels)
LogR_data <- LogR_data[,less_indices]

dim(LogR_data)

#Step 2: Creating data model

#Create Data parition with 80% as training and 20% as testing
#Use Cross validation on the training set while 
train_indices <- createDataPartition(LogR_data$Attrition 
                                   ,p = 0.8
                                   ,list = F)
logR_train <- LogR_data[train_indices,]
logR_test <- LogR_data[-train_indices,]

#Define the cross validation
tr_control <- trainControl(method = 'cv', number = 5, classProbs = T)

#Model 1 - Basic Logistic regression Model
tic('LogR_Model_1')
logR_model_1 = suppressWarnings(train(Attrition~., data=logR_train
                     , method="glm"
                     , metric="ROC"
                     , trControl=tr_control))
 
toc()

#Predict using the Logistic Regression Model 1
predict_logR1 <- suppressWarnings(
  predict(logR_model_1
          ,newdata = logR_test
          ,type = 'raw')
)

#Evaluating the model

confusionMatrix(as.factor(predict_logR1), as.factor(logR_test$Attrition))
auc(as.numeric(logR_test$Attrition), as.numeric(predict_logR1))


#Model 2 - Logistic regression Model
tic('LogR_Model_2')
logR_model_2 = train(Attrition~., data=logR_train
                                      , method="glm"
                                      , metric="Accuracy"
                                      , trControl=tr_control
                                      , tuneGrid = expand.grid(parameter=c(1, 10))
                                      )

toc()

#Predict using the Logistic Regression Model 1
predict_logR2 <- suppressWarnings(
  predict(logR_model_2
          ,newdata = logR_test
          ,type = 'raw')
)

#Evaluating the model

confusionMatrix(as.factor(predict_logR2), as.factor(logR_test$Attrition))
logR_auc <- auc(as.numeric(logR_test$Attrition), as.numeric(predict_logR2))

logR_auc

#The Accuracy with tuning does not show improvement above 67%. 