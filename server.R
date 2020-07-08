#source("global.R")
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
 
 
 
 at2 <- reactive({
   req(input$file)
   out <-readr::read_csv(input$file$datapath)
   return(out)
 })
 
 all_ids <- reactive({
   req(input$file)
   at2_in <- at2()
   out<-unique(at2_in$Symbols)
   return(out)
 })
  
 choiceList <- reactive({
   req(input$file)
   return(c(all_ids()))
 })
 
  output$view<-DT::renderDataTable({
    req(input$file)
    at2_in <- at2()
    DT::datatable(at2_in %>% filter(Symbols==as.character(input$mygene)))
  })
  
  #input$mygene
  output$summary<- renderText({
    "This application allows users to select a single gene of interest and 
    plot a interactive boxplot for gene expression data from AT-II cells"
  })
  
  output$select = renderUI({
    req(input$file)
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
   #   req(input$file)
   #    at2_in <- at2()
   #   df<-at2_in %>% filter(Gene_Symbol==as.character(input$mygene))
   #   #t<- #df %>% summarise(max(count)) %>% pull() %>% as.data.frame()
   #   #t<-as.numeric(max(df$count))
   #   set_y_axis <- list(title = "Counts (CPM)", range = c(0,(max(df$count)+0.15*max(df$count))))
   #   set_x_axis_wt <- list(title = "Juvenile AT2")
   #   
   #   # font style # %>% layout(boxmode="group")
   #   p1<-plot_ly(data=df %>% filter(genotype=="WT" & age=="Juvenile"), y = ~count, color = ~group,colors = juvenile_colors,
   #               type = "box",width = 900, height = 800,jitter = 0.3, pointpos = 0, boxpoints = 'all') %>%
   #     layout(xaxis=set_x_axis_wt,yaxis = set_y_axis)# %>% layout(annotations = juv_title)
   #   p <- subplot(p1, titleX = TRUE, titleY = TRUE,nrows = 2,margin = 0.07) %>% layout(showlegend = TRUE)
   #   p %>%
   #   config(plot_ly(),
   #          toImageButtonOptions= list(filename = as.character(input$mygene))) #,#width = 1000, #height =  350 # thi can be added to toImageButtonOptions
   # })
  
  # output$uigene = renderPlotly({
  #   # inputgene = input$mygene # updateSelectizeInput(session, 'mygene', choices  = c(unique(at2$Gene_Symbol)),
  #     #                 server   = TRUE,
  #            #          selected = "Ddx58")
  #   req(input$file)
  #    at2_in <- at2()
  #   df<-at2_in %>% filter(Gene_Symbol==as.character(input$mygene))
  #   #t<- #df %>% summarise(max(count)) %>% pull() %>% as.data.frame()
  #   #t<-as.numeric(max(df$count))
  #   set_y_axis <- list(title = "Counts (CPM)", range = c(0,(max(df$count)+0.15*max(df$count))))
  #   set_x_axis_wt <- list(title = "Juvenile AT2")
  #   set_x_axis_ko <- list(title = "Adult AT2")
  #   # font style # %>% layout(boxmode="group")
  #   p1<-plot_ly(data=df %>% filter(genotype=="WT" & age=="Juvenile"), y = ~count, color = ~group,colors = juvenile_colors,
  #               type = "box",width = 900, height = 800,jitter = 0.3, pointpos = 0, boxpoints = 'all') %>%
  #     layout(xaxis=set_x_axis_wt,yaxis = set_y_axis)# %>% layout(annotations = juv_title)
  #   p2<-plot_ly(data=df %>% filter(genotype=="WT" & age=="Adult"), y = ~count, color = ~group, colors = adult_colors,
  #               type = "box",width = 900, height = 800,jitter = 0.3, pointpos = 0, boxpoints = 'all') %>%
  #     layout(xaxis=set_x_axis_ko,yaxis = set_y_axis)# %>% layout(title=adult_title)
  #   p <- subplot(p1, p2, titleX = TRUE, titleY = TRUE,nrows = 2,margin = 0.07) %>% layout(showlegend = TRUE)
  #   p %>%
  #   config(plot_ly(),
  #          toImageButtonOptions= list(filename = as.character(input$mygene))) #,#width = 1000, #height =  350 # thi can be added to toImageButtonOptions
  # })
  #hello
})
