#Title : 707 Project - Results Interpretation
#@Author : Sanjana Rajagopala
#Start Date : November 23, 2018

library(dplyr)
library(ROSE)
library(pROC)
library(ggplot2)

library(plotROC)

#1. Combine the training data set with the prediction results of test dataset from the model with the best generalization accuracy.
#Here, the results from Extreme Gradient Boost model will be used.

#The dataset is XGBM_data. Make copy of this so that it can be used for visualization


final_data <- XGBM_Data


#2. Visualizing the performance metrics of all the models for comparison
par(mfrow = c(3,2))


plot.roc(as.numeric(logR_test$Attrition), as.numeric(predict_logR2)
         , main = 'Logistic Regression',  col='darkmagenta', print.auc = T)

plot.roc(as.numeric(rf_test$Attrition), as.numeric(predict_RF4)
         ,  main = 'Random Forest', col='darkseagreen4', print.auc = T)

plot.roc(as.numeric(svm_test$Attrition), as.numeric(predict_svm_5)
         ,  main = 'SVM',  col='firebrick3',print.auc = T)
plot.roc(as.numeric(gbm_test$Attrition), as.numeric(predict_gbm6)
         , main = 'Gradient Boost',  col='salmon4', print.auc = T)

plot.roc(as.numeric(xgbm_test$Attrition), as.numeric(predict_XGBM_2)
         ,  main = 'Best Model - XGBoost',  col='steelblue', print.auc = T)


#3. Check the features of importance from the best model
feat_importance <- varImp(XGBM_model_2)

imp_DF <- data.frame(features = row.names(feat_importance[[1]]),
                     importance_val =  round(feat_importance[[1]]$Overall, 2)
  
) 

imp_DF <- arrange(imp_DF, desc(importance_val))

#Plot the top 10 features with their importances
ggplot(head(imp_DF, 20), aes(x = reorder(features, importance_val), y = importance_val)) +
  geom_bar(stat = "identity", fill = 'tan4') + coord_flip()


#It can be observed from the plot that the most influencing features are - 
#1.Overtime
# - When employees work overtime, the probablity of employee attrition is high. This result is evident
#from the previous descriptive visualization plots 

#2. Job levels
# The occurence of employee retention is least among the employees at job level = 2.
#This mostly depicts the common cases - once the the employees at level=1, get promoted and reach the 
#level 2, the tendency is to shift to another company as the chances of getting a salary hike are high. This shows
#that measures that HR must take to influence the newly promoted employees to remain in the company (Especially, those from level 1 to level 2)

#3. Work-life balance
#The value of 3 refers to a better work-life balance.

#4. The Marital Status = Single
#Considering the cases where employees who are Single prefer exploring greater opportunities thereby,
#tend to switch jobs. In case of employees who are married prefer stability and hence, the attrition
#rate amongst such employees is lower. 

#5. Stock options
#This implies that considering the policy of providing stock options for employees will help in increasing employee retition. This is
#because of the fact that it can be oserved the employees with least stock options tend to leave the company.

#5. Number of Years served at company
#The employees who have spent the least time - 1 year with the company are the ones who tend to leave. This 
#depicts that within the period of 1 or 2 years, the employees are not influenced enough to remain in the organization.
#Hence, the HR can devise strategies that attract and influence the employees in the initial
#years itself. 

#These suggest the areas that need the HR attention. Thus, provides the direction in which 
#employee attrition can be prevented. 



