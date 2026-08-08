library(shiny)
library(shinyjs)

ui <- fluidPage(

  useShinyjs(),

  titlePanel("Análisis de Datos - Corrector Workshop"),

  sidebarLayout(

    sidebarPanel(
      img(src = "iqslogo.png", height = "35%", width = "35%"),

      helpText("Upload the file with your results and press 'Get result'
                to see how well you have done. Write your email in order
                to record your result."),

      br(),
      textInput("user", "Enter your IQS email"),

      radioButtons("filetype", "Select the type of file",
                   choices = c(Excel = "excel",
                               CSV   = "csv"),
                   selected = "excel"),

      fileInput("file1", "Choose your results file",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv", ".xlsx")),

      actionButton("button", "Get result"),
      br(), br(),

      # CSV parsing options are only relevant (and only shown) when the
      # student selects CSV; in practice almost everyone uploads Excel.
      conditionalPanel(
        condition = "input.filetype == 'csv'",
        helpText("CSV Options"),
        radioButtons("sep", "Separator",
                     choices = c(Comma     = ",",
                                 Semicolon = ";",
                                 Tab       = "\t"),
                     selected = ","),
        radioButtons("dec", "Decimal Character",
                     choices = c(Comma  = ",",
                                 Period = "."),
                     selected = ",")
      ),

      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All  = "all"),
                   selected = "head"),

      br(),
      helpText("Martori, F. 2026. V2.0", br(),
               "IQS School of Management,", br(),
               "Universitat Ramon Llull"),
    ),

    mainPanel(
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
