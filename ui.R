library(shiny)

source("global.R")
ui<-shinyUI(pageWithSidebar(
 
  headerPanel("Visualization of Gene Expression"), 
  sidebarPanel(
    div(p("Visualize gene expression in mouse datasets.")),

    br(),
   
    radioButtons("mode", "Choose mode:",
                 c("One dataset, one gene" = "one",
                   "One dataset, two genes" = "two", 
                   "Two datasets, one gene" = "three")),
  
    uiOutput('primaryData'),
    
    conditionalPanel(condition = "input.mode != 'one'",
    fileInput("file", "Upload gene expression table",
              multiple = FALSE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")), 
    fileInput("gFile", "Upload sample groups",
              multiple = FALSE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv"))
    
    ),
    radioButtons("colSel_pri", "Primary Data Symbol Column Location:",
                                                c("Column 1" = "one",
                                                  "Column 2" = "two")),
    
    uiOutput('colSelect_sec'),
    
    
    uiOutput('select_pri'),
    uiOutput('select_sec')
  ),
  

  mainPanel(
    tabsetPanel(id = "tabs",
      tabPanel("DataPri", dataTableOutput("view_pri")),
      tabPanel("DataSec", dataTableOutput("view_sec")),
      tabPanel("PlotPri",plotlyOutput("uigene_pri")),
      tabPanel("PlotSec",plotlyOutput("uigene_sec")),
      tabPanel("PlotBoth",plotlyOutput("both")),
      tabPanel("About", verbatimTextOutput("summary"))
    )
  )
))
