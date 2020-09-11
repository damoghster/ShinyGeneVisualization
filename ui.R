library(shiny)

source("global.R")
ui<-shinyUI(pageWithSidebar(
  # use_waiter(), # include dependencies
  # spin_seven_circle()
  # Application title
  headerPanel("BoxPlot for AT2 cells"), # name of the application (to be finalized :))
  sidebarPanel(
    div(p("Create a box plot for your gene in all the three age groups")),
    #selectInput("dataset", "Choose a dataset:", 
    #            choices = at2),selected = "AT2"),
    br(),
    # selectInput("genelist","Choose corresponding genelist:", choices = c("AT2_geneList","AM_geneList"),selected = "AT2_geneList"),
    uiOutput('primaryData'),
    fileInput("file", "Choose CSV File",
              multiple = FALSE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")), 
    fileInput("gFile", "Choose CSV Group File",
              multiple = FALSE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")), 
    radioButtons("colSel_pri", "Primary Data Symbol Column Location:",
                                                c("Column 1" = "one",
                                                  "Column 2" = "two")),
    
    uiOutput('colSelect_sec'),
    
    
    uiOutput('select_pri'),
    uiOutput('select_sec')
  ),
  
  # Show a summary of the dataset and an HTML table with the requested
  # number of observations
  mainPanel(
    tabsetPanel(
      tabPanel("DataPri", dataTableOutput("view_pri")),
      tabPanel("DataSec", dataTableOutput("view_sec")),
      tabPanel("PlotPri",plotlyOutput("uigene_pri")),
      tabPanel("PlotSec",plotlyOutput("uigene_sec")),
      tabPanel("PlotBoth",plotlyOutput("both")
      ),
      tabPanel("About", verbatimTextOutput("summary"))
    )
  )
))
