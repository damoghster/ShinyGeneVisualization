#source("global.R")
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)

options(shiny.maxRequestSize=1000*1024^2)
server<-shinyServer(function(input, output,session) {
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
  
  # xData <- reactive({
  #   req(input$file) 
  #   req(input$gFile)
  #   dfIn <- df()
  #   out <- names(dfIn)
  #   
  #   return(out)
  # })
  # 
  # yData <- reactive({
  #   req(input$file) 
  #   req(input$gFile)
  #   dfIn <- df()
  #   out <- as.numeric(dfIn)
  # 
  #   return(out)
  # })
  # 
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
    DT::datatable(at2_in %>% filter(at2_in[, 1]==as.character(input$mygene))) #make dynamic later
  })
  
  output$view2<-DT::renderDataTable({
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
    plot a interactive boxplot for gene expression data from AT-II cells"
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
  
  
  # output$uigene = renderPlotly({
  #   # inputgene = input$mygene # updateSelectizeInput(session, 'mygene', choices  = c(unique(at2$Gene_Symbol)),
  #     #                 server   = TRUE,
  #            #          selected = "Ddx58")
  #   req(input$file) req(input$gFile)
  #    at2_in <- at2()
  #   df<-at2_in %>% filter(Gene_Symbol==as.character(input$mygene))
  #   #t<- #df %>% summarise(max(count)) %>% pull() %>% as.data.frame()
  #   #t<-as.numeric(max(df$count))
  #   set_y_axis <- list(title = "Counts (CPM)", range = c(0,(max(df$count)+0.15*max(df$count))))
  #   set_x_axis_wt <- list(title = "Juvenile AT2")
  #   
  #   # font style # %>% layout(boxmode="group")
    # p1<-plot_ly(data=df_in,y = ~count, color = ~group,colors = juvenile_colors,
    #             type = "box",width = 900, height = 800,jitter = 0.3, pointpos = 0, boxpoints = 'all') %>%
    #   layout(xaxis=set_x_axis_wt,yaxis = set_y_axis)# %>% layout(annotations = juv_title)
    # p <- subplot(p1, titleX = TRUE, titleY = TRUE,nrows = 2,margin = 0.07) %>% layout(showlegend = TRUE)
  #   p %>%
  #   config(plot_ly(),
  #          toImageButtonOptions= list(filename = as.character(input$mygene))) #,#width = 1000, #height =  350 # thi can be added to toImageButtonOptions
  # })
  
  output$uigene = renderPlotly({
    req(input$file)
    req(input$gFile)
   data <- formattedData()
   data$Group <- as.factor(data$Group)
   p <- plot_ly(data = data, x = ~Group, y = ~Count, color = ~Group, type = "box",boxpoints = "all", jitter = .3,
                pointpos = 0) %>% layout(boxmode = "group")
  })


  # output$plot2 <- renderPlot({
  #   req(input$file)
  #   req(input$gFile)
  #   df_in <- df()
  #   # print(head(df_in %>%
  #   #          gather(Sample, Value) %>%
  #   #          mutate(Samples = gsub("\\d+", "", Sample)) %>%
  #   #          mutate(Group = factor(Group))))
  #   df_in %>%
  #     ggplot(aes(Group, Count)) +
  #     geom_boxplot() +
  #     geom_jitter(aes(color  = Samples)) +
  #     theme_bw()
  # })
})
