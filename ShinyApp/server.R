#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#

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
library(shiny)
library(shinyBS)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  #Load the data into a data frame
  
  getInputData <- function(){
    
    
    inputFile = input$inputfile 
    if(is.null(inputFile)){
      
      return(NULL)
    }
    
    employee_data <- read.csv( inputFile$datapath
                              , header = T
                              , stringsAsFactors = F
                              , fileEncoding="UTF-8-BOM"
    )
    return (employee_data)
    
  }
  
  
  preprocessing <- function(employee_data){
     #Converting the character columns into categories
     character_indices <- sapply(employee_data, is.character)
     employee_data[, character_indices] <- lapply(employee_data[,character_indices], as.factor)
     
     
     #Handling outliers
     
     #Unnecessary columns through investigation
     #1. Employee Number - Considering it as a unique employee identification number. It would not add much value to analysis
     #2. StandardHours - Considering its value = 80 across all the rows in the dataset, it can be deleted.
     
     employee_data$EmployeeNumber <- NULL
     employee_data$StandardHours <- NULL
     employee_data$EmployeeCount <- NULL
     employee_data$DailyRate <- NULL
     employee_data$Over18 <- NULL
     
     #We have reduced the number of features from 35 to useful 30 features.
     
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
     
     
     ############################################################
     #Discretization of Variables
     ############################################################
     
     #Consdiering the difference in the versions of R and arules, use the functions cut or discretize depending upon the system.
     
     #In old versions,
     employee_data$Age_level <- cut(employee_data$Age, method = "interval",breaks = 4, labels = c("Young","Thirties","Forties","Old"))
     employee_data$HourlyRate_level <- cut(employee_data$HourlyRate, method = "interval",breaks = 7, labels = c("30-40","40-50","50-60","60-70","70-80","80-90","80-100"))
     employee_data$DistanceFromHome_level <- cut(employee_data$DistanceFromHome, method = "interval",breaks = 6, labels = c("1-5","6-10","11-15","16-20","21-25","26-30"))
     employee_data$PercentSalaryHike_level <- cut(employee_data$PercentSalaryHike, method = "interval",breaks = 3, labels = c("11%-15%","16%-20%","21%-25%"))
     employee_data$YearsWithCurrManager_level <- cut(employee_data$YearsWithCurrManager, method = "interval", breaks = 6, labels  = c('0-3','4-6','7-9','10-12','13-15','16-18'))
     
     #Using SMOTE to create balanced dataset
     
     XGBM_Data <- employee_data
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
     
     return(XGBM_smote_data)
     
   }
   getTrainIndices <- function(XGBM_smote_data){
     set.seed(100)
     train_indices <- createDataPartition(XGBM_smote_data$Attrition 
                                          ,p = 0.8
                                          ,list = F)
     return(train_indices)
     }
   getTrainData <- function(XGBM_smote_data, train_indices)
   {
     xgbm_train <- XGBM_smote_data[train_indices,]
     return(xgbm_train)
     
   }
   getTestData <- function(XGBM_smote_data, train_indices)
   {
     xgbm_test <- XGBM_smote_data[-train_indices,]
     return (xgbm_test)
   }
   getXgbmGrid <- function()
   {
     xgbm_grid <- expand.grid(
       max_depth = input$maxDepth
       , subsample = input$subsample
       , nrounds  = input$nrounds
       , gamma  = input$gamma
       , colsample_bytree = input$colsample_bytree
       , min_child_weight = input$minChildWeight
       , eta = input$eta
     )
     
     return(xgbm_grid)
     
   }
      
   getModel <- function(xgbm_train, tr_control, xgbm_grid)
   {
     XGBM_model = suppressWarnings(train(Attrition~., data=xgbm_train
                                         , method="xgbTree"
                                         , verbose = F
                                         , metric="ROC"
                                         , trControl=tr_control
                                         , tuneGrid = xgbm_grid))
     return(XGBM_model) 
   }
   
   getPrediction <- function(XGBM_model, xgbm_test){
     #Predict using the XGBM Model 
     predict_XGBM <- suppressWarnings(predict(XGBM_model
                                              ,newdata = xgbm_test
     )
     )
     return(predict_XGBM)
   }
   
  
  
  #Evaluating the model
  
  output$aucPlot <- renderPlot({
    employee_data <- getInputData()
    if(is.null(employee_data))
    {
      output$data_alert <- renderText({ 
        paste("Steps", "1. Change the Parameters", "2. Upload the CSV file and wait for few minutes for the model training"
              , sep="\n")
              })
    }
    else
    {
    XGBM_smote_data <- preprocessing(employee_data)
    train_indices <- getTrainIndices(XGBM_smote_data)
    xgbm_train <- getTrainData(XGBM_smote_data,train_indices)
    xgbm_test <- getTestData(XGBM_smote_data,train_indices)
    
    #Define the cross validation
    tr_control <- trainControl(method = 'cv', number = 5, classProbs = T)
    xgbm_grid <- getXgbmGrid()
    XGBM_model <- getModel(xgbm_train, tr_control, xgbm_grid)
    
    predict_XGBM <- getPrediction(XGBM_model, xgbm_test)
    
    
    plot.roc(as.numeric(xgbm_test$Attrition), as.numeric(predict_XGBM)
             ,  main = 'Best Model - XGBoost',  col='steelblue', print.auc = T)
    
    }
    })
  output$varImpPlot <- renderPlot({
    employee_data <- getInputData()
    if(is.null(employee_data))
    {
      output$data_alert <- renderText({ 
        paste("Steps", "1. Change the Parameters", "2. Upload the CSV file and wait for few minutes for the model training"
              , sep="\n")
        })
    }
    else
    {
      XGBM_smote_data <- preprocessing(employee_data)
      train_indices <- getTrainIndices(XGBM_smote_data)
      xgbm_train <- getTrainData(XGBM_smote_data,train_indices)
      xgbm_test <- getTestData(XGBM_smote_data,train_indices)
      
      #Define the cross validation
      tr_control <- trainControl(method = 'cv', number = 5, classProbs = T)
      xgbm_grid <- getXgbmGrid()
      XGBM_model <- getModel(xgbm_train, tr_control, xgbm_grid)
      
      feat_importance <- varImp(XGBM_model)
      imp_DF <- data.frame(features = row.names(feat_importance[[1]]),
                           importance_val =  round(feat_importance[[1]]$Overall, 2)) 
      
      imp_DF <- arrange(imp_DF, desc(importance_val))
      
      #Plot the top 10 features with their importances
      ggplot(head(imp_DF, 20), aes(x = reorder(features, importance_val), y = importance_val)) +
        geom_bar(stat = "identity", fill = 'tan4') + coord_flip()
      
    }
    })
  
  output$downloadModel <- downloadHandler(
    
    filename = function () {
      paste("xgboost.RData")
    },
    content = function(file) {
      employee_data <- getInputData()
      if(is.null(employee_data))
      {
        output$data_alert <- renderText({ 
          
          paste("Steps", "1. Change the Parameters", "2. Upload the CSV file and wait for few minutes for the model training"
                , sep="\n")
        })
      }
      else
      {
      XGBM_smote_data <- preprocessing(employee_data)
      train_indices <- getTrainIndices(XGBM_smote_data)
      xgbm_train <- getTrainData(XGBM_smote_data,train_indices)
      xgbm_test <- getTestData(XGBM_smote_data,train_indices)
      
      #Define the cross validation
      tr_control <- trainControl(method = 'cv', number = 5, classProbs = T)
      xgbm_grid <- getXgbmGrid()
      XGBM_model <- getModel(xgbm_train, tr_control, xgbm_grid)
      
      save(XGBM_model,file=file)
      }
    }
  )
      
    
    
})
