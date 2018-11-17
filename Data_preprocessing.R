#Title : 707 Project 
#@Author : Sanjana Rajagopala
#Start Date : October 31, 2018

#Load the required libraries
library(caret)
library(dplyr)


#Read the data

employee_data <- read.csv('C://Users/sanja/Desktop/Courses_Fall2018/IST707/Project/IBM_Employee_Attrition_Dataset.csv'
                          , header = T
                          , stringsAsFactors = F
                          )
#View the data
View(employee_data)


#Investigate the dataset
str(employee_data)

#Checking the rows and columns in the dataset
dim(employee_data)

#Converting the character columns into categories
character_indices <- sapply(employee_data, is.character)
employee_data[, character_indices] <- lapply(employee_data[,character_indices], as.factor)

str(employee_data)


employee_data$Age <- employee_data$ï..Age
employee_data <- select(employee_data, -ï..Age) 

#Checking for NA/missing values
sum(!complete.cases(employee_data))
#Shows the presence of 1470 complete data 

#Finding out the columns with NAs/missing values
colSums(is.na(employee_data))
#Shows that none of the columns have NA values


#Checking for Duplicate entries
nrow(employee_data[!(duplicated(employee_data)),])
nrow(employee_data)

#Handling outliers

#In case of categorical variables, check the number of entries in each category so that the values outside
categorical_indices <- sapply(employee_data, is.factor)
outlier_data <- lapply(employee_data[, categorical_indices], table)

#Numerical values
#TotalWorkingYears


#To check the outliers using boxplot
employee_data$TotalWorkingYears[employee_data$TotalWorkingYears %in% boxplot.stats(employee_data$TotalWorkingYears)$out]<- median(employee_data$TotalWorkingYears)
boxplot(employee_data$TotalWorkingYears)


boxplot(employee_data$TrainingTimesLastYear)

boxplot(employee_data$YearsInCurrentRole)
boxplot(employee_data$YearsSinceLastPromotion)
employee_data$YearsSinceLastPromotion[employee_data$YearsSinceLastPromotion %in% boxplot.stats(employee_data$YearsSinceLastPromotion)$out]<- median(employee_data$YearsSinceLastPromotion)
boxplot(employee_data$YearsSinceLastPromotion)

boxplot(employee_data$YearsWithCurrManager)


#Unnecessary columns through investigation
#1. Employee Number - Considering it as a unique employee identification number. It would not add much value to analysis
#2. StandardHours - Considering its value = 80 across all the rows in the dataset, it can be deleted.

employee_data$EmployeeNumber <- NULL
employee_data$StandardHours <- NULL
employee_data$EmployeeCount <- NULL
employee_data$DailyRate <- NULL
employee_data$Over18 <- NULL

num_indices <- sapply(employee_data, is.numeric)
stats_summary <- lapply(employee_data[,num_indices], summary)

stats_summary

#Conversion of features into categorical type
employee_data$Education <- as.factor(employee_data$Education)
employee_data$EnvironmentSatisfaction <- as.factor(employee_data$EnvironmentSatisfaction)
employee_data$JobInvolvement <- as.factor(employee_data$JobInvolvement)
employee_data$JobSatisfaction <- as.factor(employee_data$JobSatisfaction)
employee_data$PerformanceRating <- as.factor(employee_data$PerformanceRating)
employee_data$RelationshipSatisfaction <- as.factor(employee_data$RelationshipSatisfaction)
employee_data$WorkLifeBalance <- as.factor(employee_data$WorkLifeBalance)
employee_data$JobLevel <- as.factor(employee_data$JobLevel)
employee_data$StockOptionLevel <- as.factor(employee_data$StockOptionLevel)


#Normalization of numerical data

#Maintain the separate copies if normalized and not-bnormalized data  - so that modelling is done easily.
normalized_empl_data <- employee_data

normalized_empl_data$MonthlyIncome <- scale(normalized_empl_data$MonthlyIncome)
normalized_empl_data$NumCompaniesWorked <- scale(normalized_empl_data$NumCompaniesWorked)
normalized_empl_data$PercentSalaryHike <- scale(normalized_empl_data$PercentSalaryHike)
normalized_empl_data$TotalWorkingYears <- scale(normalized_empl_data$TotalWorkingYears)
normalized_empl_data$YearsAtCompany <- scale(normalized_empl_data$YearsAtCompany)
normalized_empl_data$TrainingTimesLastYear <- scale(normalized_empl_data$TrainingTimesLastYear)
normalized_empl_data$YearsInCurrentRole <- scale(normalized_empl_data$YearsInCurrentRole)
normalized_empl_data$YearsSinceLastPromotion <- scale(normalized_empl_data$YearsSinceLastPromotion)
normalized_empl_data$YearsWithCurrManager <- scale(normalized_empl_data$YearsWithCurrManager)
normalized_empl_data$MonthlyIncome <- scale(normalized_empl_data$MonthlyIncome)


############################################################
#Code for Discretization of Variables
############################################################
#Age
#Hourly Rate
#DistanceFromHome
#PercentSalaryHike
#YearsWithCurrentManager
##################################################################################################################################33
employee_data$Age_level <- discretize(employee_data$Age, method = "interval",breaks = 4, labels = c("Young","Thirties","Forties","Old"))
employee_data$HourlyRate_level <- discretize(employee_data$HourlyRate, method = "interval",breaks = 7, labels = c("30-40","40-50","50-60","60-70","70-80","80-90","80-100"))
employee_data$DistanceFromHome_level <- discretize(employee_data$DistanceFromHome, method = "interval",breaks = 6, labels = c("1-5","6-10","11-15","16-20","21-25","26-30"))
employee_data$PercentSalaryHike_level <- discretize(employee_data$PercentSalaryHike, method = "interval",breaks = 3, labels = c("11%-15%","16%-20%","21%-25%"))
employee_data$YearsWithCurrManager_level <- cut(employee_data$YearsWithCurrManager, method = "interval", breaks = 6, labels  = c('0-3','4-6','7-9','10-12','13-15','16-18'))

