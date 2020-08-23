#source("global.R")
library(dplyr)
library(tidyr)
library(ggplot2)
options(shiny.maxRequestSize = 1000 * 1024 ^ 2)
server <- shinyServer(function(input, output, session) {
  # to be modified to incorporate multiple datasets later
  #   Return the requested dataset
  # datasetInput <- AT2_dat#reactive({
  #switch(input$dataset,
  #       "AT2" = AT2_dat)#, # aging flu pilot data taken from fsmresfiles
  #"AM_dat" = AM_dat)
  #  })
  # genelistInput<-reactive({
  #    switch (input$genelist,
  #           "AT2_genes" = AT2_geneList,
  #          "AM_genes" = AM_geneList
  #   )
  #})
  
  
  selNum <- reactive({
    out <- switch(input$colSel,
                  one = 1, two = 2)
    return(out)
  })
 
  
  groups <- reactive({
    req(input$gFile)
    out <- readr::read_csv(input$gFile$datapath)
    return(out)
  })
  
  all_ids <- reactive({
    req(input$file)
    req(input$gFile)
    primary <- primary()
    selectionNum <- selNum()
    out <-
      unique(primary[, selectionNum]) #change 1 to variable that can be selected by user
    return(out)
  })
  
  choiceList <- reactive({
    req(input$file)
    req(input$gFile)
    return(c(all_ids()))
  })
  
  
  
  
  primary <- reactive({
   
    infile <- input$data
    if (is.null(infile)){
      return(NULL)
    }
    out <- readr::read_csv(paste0('./Primary/',infile))
    return(out)
  })
  
  dfPri <- reactive({
   
    primary <- primary()
    selectionNum <- selNum()
    out <-
      primary %>% filter(primary[, selectionNum] == as.character(input$mygene)) %>% distinct(colnames(primary, do.NULL = TRUE, prefix = "col")[1], .keep_all = TRUE) %>% select(3:ncol(primary))
    return(out)
  })
  
  xDataPri <- reactive({
    
    dfIn <- dfPri()
    out <- names(dfIn)
    
    return(out)
  })
  
  yDataPri <- reactive({
   
    dfIn <- dfPri()
    out <- as.numeric(dfIn)
    return(out)
  })
  
  
  
  at2 <- reactive({
    req(input$file)
    req(input$gFile)
    out <- readr::read_csv(input$file$datapath)
    return(out)
  })
  
  
  dfSec <- reactive({
    req(input$file)
    req(input$gFile)
    at2_in <- at2()
    selectionNum <- selNum()
    out <-
      at2_in %>% filter(at2_in[, selectionNum] == as.character(input$mygene)) %>% distinct(colnames(at2_in, do.NULL = TRUE, prefix = "col")[1], .keep_all = TRUE) %>% select(3:ncol(at2_in))
    return(out)
  })
  
  xDataSec <- reactive({
    req(input$file)
    req(input$gFile)
    dfIn <- dfSec()
    out <- names(dfIn)
    
    return(out)
  })
  yDataSec <- reactive({
    req(input$file)
    req(input$gFile)
    dfIn <- dfSec()
    out <- as.numeric(dfIn)
    
    return(out)
  })
  
  output$view <- DT::renderDataTable({
    req(input$file)
    req(input$gFile)
    at2_in <- at2()
    DT::datatable(at2_in %>% filter(at2_in[, 1] == as.character(input$mygene))) #make dynamic later
  })
  
  
  #input$mygene
  output$summary <- renderText({
    "This application allows users to select a single gene of interest and
    plot a interactive boxplot for gene expression data from AT-II cells"
  })
  
  output$select = renderUI({
    req(input$file)
    req(input$gFile)
    selectizeInput(
      "mygene",
      label = h5("Enter your gene here : "),
      multiple = FALSE,
      selected = NULL,
      choices =  choiceList()
    )
  })
  
  output$primaryData = renderUI({
    print(getwd())
    print(ls())
    selectInput(inputId = 'data',
                label = 'Choose primary data',
                choices = list.files(path = "./Primary",
                                     full.names = FALSE,
                                     recursive = FALSE))
  })
  
  
 
  
  
  output$plotPrimary = renderPlotly({
    xDataIn <- xDataPri()
    yDataIn <- yDataPri()
    p <-
      plot_ly(
        x = xDataIn,
        y = yDataIn,
        name = "Gene Counts",
        type = "scatter"
      )
  })
  
  output$plotSecondary = renderPlotly({
    req(input$file)
    req(input$gFile)
    xDataIn <- xDataSec()
    yDataIn <- yData()
    p <-
      plot_ly(
        x = xDataIn,
        y = yDataIn,
        name = "Gene Counts",
        type = "scatter"
      )
  })
  
  # output$plot2 <- renderPlot({
  #   req(input$file)
  #   req(input$gFile)
  #   df_in <- dfSec()
  #   group_in <- groups()
  #   df_in %>%
  #     gather(Sample, Value) %>%
  #     mutate(Samples = gsub("\\d+", "", Sample)) %>%
  #     left_join(group_in) %>%
  #     mutate(Group = factor(Group)) %>%
  #     ggplot(aes(Group, Value)) +
  #     geom_boxplot() +
  #     geom_jitter(aes(color  = Samples)) +
  #     theme_bw()
  # })
  #
  # p<- reactive({
  #  out<- plot_ly(x=xData, y=yData,name = "Gene Counts", type = "bar")
  #  return(out)
  # })
})
