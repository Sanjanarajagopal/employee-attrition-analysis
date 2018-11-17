#Load the required libraries
library(caret)
library(dplyr)
library(arules)

########################################3
#Visualizations
####################################################
#1. How does attrition vary across each Departments and environment satisfaction? - Categorised Bar charts
##########################################################
employee_data <- normalized_empl_data
dept_attr_data <- tapply(as.numeric(employee_data$Attrition), list(employee_data$Department,employee_data$EnvironmentSatisfaction), FUN = sum)
dept_attr_data

colnames(dept_attr_data) <- c('Department', 'EnvironmentSatisfaction', 'Attrition')

barplot(t(dept_attr_data)
        ,beside = T
        , col = c('green4', 'red4', 'tan4', 'grey')
        
        )
plot(employee_data$HourlyRate, employee_data$NumCompaniesWorked)
