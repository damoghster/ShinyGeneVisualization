#source("global.R")
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)

options(shiny.maxRequestSize=1000*1024^2)
server<-shinyServer(function(input, output,session) {
  
  selNum<- reactive({
    out<- switch(input$colSel,
                  one = 1, two = 2)
    return(out)
  })
  
  at2 <- reactive({
    req(input$file) 
    req(input$gFile)
    out <-readr::read_csv(input$file$datapath)
    return(out)
  })
  
  groups <- reactive({
    req(input$gFile)
    out <-readr::read_csv(input$gFile$datapath)
    return(out)
  })
  
  all_ids <- reactive({
    req(input$file) 
    req(input$gFile)
    at2_in <- at2()
    selectionNum <- selNum()
    out<-unique(at2_in[,selectionNum]) #change 1 to variable that can be selected by user
    return(out)
  })
  
  choiceList <- reactive({
    req(input$file) 
    req(input$gFile)
    return(c(all_ids()))
  })
  
  
  df <- reactive({
    req(input$file) 
    req(input$gFile)
    at2_in <- at2()
    selectionNum <- selNum()
     out <- at2_in %>% filter(at2_in[,selectionNum]==as.character(input$mygene)) %>% distinct(colnames(at2_in, do.NULL = TRUE, prefix = "col")[1], .keep_all = TRUE) %>% select(3:ncol(at2_in))
     return(out)
  })
  
  formattedData <- reactive({
    req(input$file)
    req(input$gFile)
    df_in <- df()
    group_in <- groups()
    df_in <- as.data.frame(t(df_in)) 
    colnames(df_in) <- c("Count")
    group_in <- as.data.frame(t(group_in))
    colnames(group_in) <- c("Group")
    group_in$Sample = row.names(group_in)
    print(df_in)
    print(group_in)
    df_in <- cbind(df_in,group_in)
    # df_in %>% group_by(Sample)
    print(df_in)
    out <-df_in
    return(out)
  })
  
  output$view<-DT::renderDataTable({
    req(input$file) 
    req(input$gFile)
    at2_in <- at2()
    selectionNum <- selNum()
    f <- formattedData()
    DT::datatable(f)
  })
  
  #input$mygene
  output$summary<- renderText({
    "This application allows users to select a single gene of interest and 
    plot a interactive boxplot for gene expression data"
  })
  
  output$select = renderUI({
    req(input$file) 
    req(input$gFile)
    selectizeInput("mygene",
                   label = h5("Enter your gene here : "),
                   multiple = FALSE,
                   selected = NULL,
                   choices =  choiceList())
  })
  
  output$uigene = renderPlotly({
    req(input$file)
    req(input$gFile)
   data <- formattedData()
   data$Group <- as.factor(data$Group)
   p <- plot_ly(data = data, x = ~Group, y = ~Count, color = ~Group, type = "box",boxpoints = "all", jitter = .3,
                pointpos = 0) %>% layout(boxmode = "group")
  })

})
