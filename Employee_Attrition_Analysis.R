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

#Step 1  - DATA PRE-PROCESSING

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

#Step 2 - DATA EXPLORATION & DESCRIPTIVE STATISTICS

num_indices <- sapply(employee_data, is.numeric)
stats_summary <- lapply(employee_data[,num_indices], summary)

stats_summary

#Using boxplot and histogram to determine the distribution of numerical vectors
boxplot(stats_summary$DailyRate)

#Some questions to investigate
#1. How does attrition vary across each Departments with an employee count and environment satisfaction? - Categorised Bar charts
#2. Do HourlyRate, job satisfaction, job involvement affect each other? If so how? - Scatter plot, with size or color as third parameter
#3. Show performance rating groups with attrition
#4. Showing distributions of - 
    #a. BUsiness Travel
    #b. Age - so that it can be bucketized
    #c. MaritalStatus with RelationshipSatisfaction
    #d. TotalWorkingHours
    #e. Work life balance, distance from home
    #f. Year at a company
#5. Years with Manager, Percentage Hike

