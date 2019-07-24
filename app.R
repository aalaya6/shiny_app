

library("shiny")

# Define ui function
ui <- shinyUI(
  fluidPage(
    
    titlePanel("POS tagging"),
    
    sidebarLayout( 
      
      sidebarPanel(  
        
        fileInput("file", "Upload text file"),
        checkboxGroupInput("variable", "Fiter POS of interest:",
                           c("Noun" = "NOUN",
                             "Adjective" = "ADJ",
                             "Verb" = "VERB",
                             "AdVerb" = "",
                             "Conjunction"="CCONJ",
                             "Pronoun"="PRON",
                             "Proper Noun"="PROPN"
                           ),c("NOUN","ADJ","PROPN")),
        tableOutput("data"),
        
        numericInput('clusters', 'Number of Clusters', 3,
                     min = 1, max = 9)     ),   # end of sidebar panel
      
      
      mainPanel(
        
        tabsetPanel(type = "tabs",
                    
                    tabPanel("Overview",
                             h4(p("Data input")),
                             p("This app supports txt file",align="justify"),
                             br(),
                             h4('Please upload text file for which POS tagging needs to be done'),
                             p('Use the option ', 
                               span(strong("Upload text file")),
                               'provided to your left and upload the txt data file. You can also select the parts of speech you are interested to analyze by checking the appropriate  check boxes')),
                    tabPanel("Annotated documents", 
                             dataTableOutput('table1')),
                    
                    tabPanel("Wordclouds",
                             plotOutput('plot1')),
                    
                    tabPanel("Cooccurence",
                             plotOutput('plot2'))
                    
        ) # end of tabsetPanel
      )# end of main panel
    ) # end of sidebarLayout
  )  # end if fluidPage
) # end of UI



# Define Server function
server <- shinyServer(function(input, output) {
  
  Dataset <- reactive({
    if (is.null(input$file)) { return(NULL) }
    else{
      library("stringr")
      require(stringr)
      doc=readLines(input$file$datapath)
      str(doc)
      library("udpipe")
      require(udpipe)
      english_model = udpipe_download_model("english") 
      y <- udpipe_load_model(english_model)
      x <- udpipe_annotate(y, x = doc)
      x <- as.data.frame(x)
      library("dplyr")
      require(dplyr)
      x<-x[,-4]
      x<-x %>% filter(x$upos %in% input$variable)
      return(x)
    }
  })
  
  output$table1 = renderDataTable({ 
    b<-Dataset()
    b[1:100,]
  })
  
  word_cloud <- reactive({
    if (is.null(input$file)) { return(NULL) }
    else{
      suppressPackageStartupMessages({
        if (!require(udpipe)){install.packages("udpipe")}
        if (!require(stringr)){install.packages("stringr")}
        if (!require(wordcloud)){install.packages("wordcloud")}
      })
      library("stringr")
      require(stringr)
      doc=readLines(input$file$datapath)
      str(doc)
      library("udpipe")
      require(udpipe)
      english_model = udpipe_download_model("english") 
      y <- udpipe_load_model(english_model)
      x <- udpipe_annotate(y, x = doc)
      x <- as.data.frame(x)
      library("dplyr")
      require(dplyr)
      x<-x[,-4]
      word_cloud_data<-x %>% filter(x$upos %in% c("NOUN","VERB"))
      return(word_cloud_data)
      
    }
  })
  
  clusters <- reactive({
    kmeans(Dataset(), input$clusters)
  })
  
  noun_plot <- reactive({
    suppressPackageStartupMessages({
      if (!require(udpipe)){install.packages("udpipe")}
      if (!require(stringr)){install.packages("stringr")}
      if (!require(wordcloud)){install.packages("wordcloud")}
    })
    library("wordcloud")
    require(wordcloud)
    a=word_cloud()
    a<-a %>% filter(a$upos=="NOUN")
    wordcloud(a$token,min.freq = 1,max.words = 200,scale = c(3.5,0.25),colors = brewer.pal(6, "Dark2"))
    
  })
  verb_plot <- reactive({
    suppressPackageStartupMessages({
      if (!require(udpipe)){install.packages("udpipe")}
      if (!require(stringr)){install.packages("stringr")}
      if (!require(wordcloud)){install.packages("wordcloud")}
    })
    library("wordcloud")
    require(wordcloud)
    a=word_cloud()
    a<-a %>% filter(a$upos=="VERB")
    wordcloud(a$token,min.freq = 1,max.words = 200,scale = c(3.5,0.25),colors = brewer.pal(6, "Dark2"))
    
  })
  
  
  output$plot1 = renderPlot({
    par(mfrow=c(1,2))
    noun_plot()
    verb_plot()
  })
  
  output$plot2 = renderPlot({
    doc_cooc <- cooccurrence(
      x=Dataset(), 
      term = "lemma", 
      group = c("doc_id"))
    suppressPackageStartupMessages({
      if (!require(igraph)){install.packages("igraph")}
      if (!require(ggraph)){install.packages("ggraph")}
      if (!require(ggplot2)){install.packages("ggplot2")}
    })
    
    library(igraph)
    library(ggraph)
    library(ggplot2)
    
    net <- head(doc_cooc, 30)
    net <- igraph::graph_from_data_frame(net)
    
    ggraph(net, layout = "fr") +  
      
      geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "red") +  
      geom_node_text(aes(label = name), col = "blue", size = 10) +
      
      theme_graph(base_family = "Arial Narrow") +  
      theme(legend.position = "none") +
      
      labs(title = "Cooccurrences within 3 words distance")
    
  })
  
})

shinyApp(ui = ui, server = server)