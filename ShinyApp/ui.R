#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Employee Attrition Analysis"),
  
  # Sidebar with a slider input for number of bins 
  
  sidebarLayout(
    sidebarPanel(
      
      numericInput("maxDepth", "Maximum Depth : ", 15), br(), 
      numericInput("subsample", "Sub Sample : ", 0.9), br(),
      numericInput("nrounds", "Number of Rounds : ", 500), br(),
      numericInput("gamma", "Gamma : ", 0.8), br(),
      numericInput("colsample_bytree", "Columns Sample by Tree : ", 0.7), br(),
      numericInput("minChildWeight", "Minimum Child Weight : ", 1), br(),
      numericInput("eta", "ETA : ", 0.03), br(),
      
      fileInput('inputfile', 'Choose the Data CSV File',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
      
      downloadButton('downloadModel', "Download RData")
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      textOutput('data_alert'),
      plotOutput('aucPlot'),
      plotOutput('varImpPlot')
      
      )

  )
))

