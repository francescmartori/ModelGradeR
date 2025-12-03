library(shiny)

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Análisis de Datos - Corrector Workshop"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      img(src = "iqslogo.png", height="35%", width="35%"),
      
      helpText("Upload the file with your results and press 'Get Result' to see how well you have done. Write your email in order to record your result." ),
      
      br(),
      # Input: User
      textInput("user", "Enter your IQS email"),
      
      # Input: File type
      radioButtons("filetype", "Select the type of file",
                   choices = c(Excel = "excel",
                               CSV = "csv"),
                   selected = "excel"),
      
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv", ".xlsx")),
      
      # Input: Select quotes ----
      actionButton("button", "Get result"),
      br(), br(),
      actionButton("updateChart", "Update Chart"),
      br(),   br(),  
      
      helpText("Advanced Options" ),
      
      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      # Input: decimal separator
      radioButtons("dec", "Decimal Character",
                   choices = c(Comma = ",",
                               Period = "."),
                   selected = ","),
      
      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head"),
      
      br(),
      passwordInput("admin_pw", "Enter password to update", value = ""),
      helpText("Martori, F. 2025. V1.0",br(),
               "IQS School of Management,",br(), 
               "Universitat Ramon Llull" ),
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Data file ----
      br(), 
      br(),
      uiOutput("Answer"),
      br(), 
      plotOutput("chart"),
      br(), 
      br(),
      tableOutput("contents")
    )
    
  )
)
