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
    radioButtons("colSel", "Symbol Column Location:",
                                                c("Column 1" = "one",
                                                  "Column 2" = "two")),
    
    uiOutput('select')
  ),
  
  # Show a summary of the dataset and an HTML table with the requested
  # number of observations
  mainPanel(
    tabsetPanel(
      tabPanel("Data", dataTableOutput("view")),
      tabPanel("Data", dataTableOutput("view2")),
      tabPanel("Plot",plotlyOutput("uigene")),
      tabPanel("PlotGrouped",plotOutput("plot2")),
      tabPanel("About", verbatimTextOutput("summary"))
      
    )
  )
))
