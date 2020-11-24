#source("global.R")
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)

options(shiny.maxRequestSize=1000*1024^2)
server<-shinyServer(function(input, output,session) {
  
  observeEvent(input$mode, {
    out<- switch(input$mode,
                    one = 1, two = 2, three = 3)
    if(out == 1){
    hideTab(inputId = "tabs", target = "DataSec")
      hideTab(inputId = "tabs", target = "PlotSec")
       hideTab(inputId = "tabs", target = "PlotBoth")
    }
    
  })
  
  observeEvent(input$mode, {
    out<- switch(input$mode,
                 one = 1, two = 2, three = 3)
    if(out != 1){
      showTab(inputId = "tabs", target = "DataSec")
      showTab(inputId = "tabs", target = "PlotSec")
      showTab(inputId = "tabs", target = "PlotBoth")
    }
  })
  
  selNum_pri<- reactive({
    out<- switch(input$colSel_pri,
                  one = 1, two = 2)
    return(out)
  })
  selNum_sec<- reactive({
    out<- switch(input$colSel_sec,
                 one = 1, two = 2)
    return(out)
  })
  mode<- reactive({
    out<- switch(input$mode,
                 one = 1, two = 2)
    return(out)
  })
  data_pri <- reactive({
    infile <- input$priData
    if (is.null(infile)){
      return(NULL)
    }
    print(infile)
    path <- file.path(paste0('./Primary/',infile,'/Data.csv'))
    out <- readr::read_csv(path)
    return(out)
  })
  
  title_pri <- reactive({
    infile <- input$priData
    if (is.null(infile)){
      return(NULL)
    }
    out <- infile
    return(out)
  })
  
  
  groups_pri <- reactive({
    infile <- input$priData
    if (is.null(infile)){
      print("hello")
      return(NULL)
    }
    print(infile)
    path <- file.path(paste0('./Primary/',infile,'/GroupFile.csv'))
    out <- readr::read_csv(path)
    return(out)
  })
  
  all_ids_pri <- reactive({
    data_in <- data_pri()
    selectionNum <- selNum_pri()
    out<-unique(data_in[,selectionNum]) #change 1 to variable that can be selected by user
    return(out)
  })
  
  choiceList_pri <- reactive({
    return(c(all_ids_pri()))
  })
  
  
  df_pri<- reactive({
    data_in <- data_pri()
    selectionNum <- selNum_pri()
    out <- data_in %>% filter(data_in[,selectionNum]==as.character(input$mygene_pri)) %>% distinct(colnames(data_in, do.NULL = TRUE, prefix = "col")[1], .keep_all = TRUE) %>% select(3:ncol(data_in))
    return(out)
  })
  
  formattedData_pri <- reactive({
    df_in <- df_pri()
    group_in <- groups_pri()
    df_in <- as.data.frame(t(df_in))
    colnames(df_in) <- c("Count")
    group_in <- as.data.frame(t(group_in))
    colnames(group_in) <- c("Group")
    group_in$Sample = row.names(group_in)
    print(df_in)
    print(group_in)
    df_in <- cbind(df_in,group_in)
    # df_in %>% group_by(Sample)
    print(head(df_in))
    out <-df_in
    return(out)
  })
  
  ########################
  data_sec <- reactive({
    req(input$file) 
    req(input$gFile)
    print(paste0("sdflsdkflj ",input$file$name))
    out <-readr::read_csv(input$file$datapath)
    return(out)
  })
  
  title_sec <- reactive({
    req(input$file) 
    req(input$gFile)
    out <- input$file$nam
    return(out)
  })
  
  groups_sec <- reactive({
    req(input$gFile)
    out <-readr::read_csv(input$gFile$datapath)
    return(out)
  })
  
  all_ids_sec <- reactive({
    req(input$file) 
    req(input$gFile)
    data_in <- data_sec()
    selectionNum <- selNum_sec()
    out<-unique(data_in[,selectionNum]) #change 1 to variable that can be selected by user
    return(out)
  })
  
  choiceList_sec <- reactive({
    req(input$file) 
    req(input$gFile)
    return(c(all_ids_sec()))
  })
  
  
  df_sec <- reactive({
    req(input$file) 
    req(input$gFile)
    data_in <- data_sec()
    selectionNum <- selNum_sec()
     out <- data_in %>% filter(data_in[,selectionNum]==as.character(input$mygene_sec)) %>% distinct(colnames(data_in, do.NULL = TRUE, prefix = "col")[1], .keep_all = TRUE) %>% select(3:ncol(data_in))
     return(out)
  })
  
  
  
  
  formattedData_sec <- reactive({
    req(input$file)
    req(input$gFile)
    df_in <- df_sec()
    group_in <- groups_sec()
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
  
  output$view_sec<-DT::renderDataTable({
    req(input$file) 
    req(input$gFile)
    f <- formattedData_sec()
    DT::datatable(f)
  })
  
  output$view_pri<-DT::renderDataTable({
    f <- formattedData_pri()
    DT::datatable(f)
  })

  output$summary<- renderText({
    "This application allows users to select a single gene of interest and 
    plot a interactive boxplot for gene expression data"
  })
  
  output$primaryData = renderUI({
    selectInput(inputId = 'priData',
                label = 'Choose primary data set',
                choices = list.files(path = "./Primary",
                                     full.names = FALSE,
                                     recursive = FALSE))
  })
  
  
  output$select_pri = renderUI({
    selectizeInput("mygene_pri",
                   label = h5("Select primary data gene: "),
                   multiple = FALSE,
                   selected = NULL,
                   choices =  choiceList_pri())
  })
  
  output$colSelect_sec = renderUI({
    req(input$file) 
    req(input$gFile)
  radioButtons("colSel_sec", "Secondary Data Symbol Column Location:",
               c("Column 1" = "one",
                 "Column 2" = "two"))
  })
  
  output$select_sec = renderUI({
    selectizeInput("mygene_sec",
                   label = h5("Select secondary data gene: "),
                   multiple = FALSE,
                   selected = NULL,
                   choices =  choiceList_sec())
  })
  
  output$uigene_pri = renderPlotly({
    data <- formattedData_pri()
    name <- title_pri()
    data$Group <- as.factor(data$Group)
    p <- plot_ly(data = data, x = ~Group, y = ~Count, color = ~Group, type = "box",boxpoints = "all", jitter = .3,
                 pointpos = 0) %>% layout(boxmode = "group") %>% layout(title = name)
  })
  
  
  output$uigene_sec = renderPlotly({
    req(input$file)
    req(input$gFile)
   data <- formattedData_sec()
   name <- title_sec()
   data$Group <- as.factor(data$Group)
   p <- plot_ly(data = data, x = ~Group, y = ~Count, color = ~Group, type = "box",boxpoints = "all", jitter = .3,
                pointpos = 0) %>% layout(boxmode = "group") %>% layout(title = name)
  })
  
  
  output$both = renderPlotly({
    req(input$file)
    req(input$gFile)
    data_pri <- formattedData_pri()
    data_pri$Group <- as.factor( data_pri$Group)
    name_pri <- title_pri()
    pri <- plot_ly(data =  data_pri, x = ~Group, y = ~Count, color = ~Group, type = "box",boxpoints = "all", jitter = .3,
                 pointpos = 0) %>% layout(boxmode = "group") 
    data_sec <- formattedData_sec()
    data_sec$Group <- as.factor(data_sec$Group)
    name_sec <- title_sec()
    sec <- plot_ly(data = data_sec, x = ~Group, y = ~Count, color = ~Group, type = "box",boxpoints = "all", jitter = .3,
                 pointpos = 0) %>% layout(boxmode = "group") %>% layout(title = paste0(name_pri,"(left)                           ", name_sec,"(right)"))
    p <- subplot(pri,sec)
    p
  })

})
