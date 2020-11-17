# Packages
library(pacman)
p_load(shiny, 
       tidyverse, 
       shinydashboard, 
       shinythemes, 
       Rtsne, 
       tm, 
       SnowballC, 
       wordcloud, 
       visNetwork, 
       RColorBrewer, 
       readr, 
       colourpicker, 
       wordcloud2, 
       word2vec,
       fastTextR, 
       text2vec,
       ggplot2,
       plotly)

# Sentida
#if(!require("devtools")) install.packages("devtools")
#devtools::install_github("Guscode/Sentida")
library(Sentida)

# Wordcloud2
#require(devtools)
#install_github("lchiffon/wordcloud2")

# Test wordcloud med denne model
#model_CoNLL17 <- read.word2vec(file = "CoNLL17_model/model.bin", normalize = TRUE)

# Find different words
# #calc_antonym <- function(word, embedding_matrix, n = 5) {
#   similarities <- embedding_matrix[word, , drop = FALSE] %>%
#     sim2(embedding_matrix, y = ., method = "cosine")
#   similarities[,1] %>% sort(decreasing = TRUE) %>% tail(n)
# }

# Matrix model
model_CoNLL17 <- read.word2vec(file = "CoNLL17_model/model.bin", normalize = TRUE)
matrix_model_CoNLL17 <- as.matrix(model_CoNLL17)

# Make tsne model of the CoNLL17 to be able to plot it 
tsne <- Rtsne(matrix_model_CoNLL17[2:500,], perplexity = 50, pca = FALSE)
tsne_plot <- tsne$Y %>%
  as.data.frame() %>%
  mutate(word = row.names(matrix_model_CoNLL17)[2:500]) %>%
  ggplot(aes(x = V1, y = V2, label = word)) + 
  geom_text(size = 3)
tsne_plot

# Wordcloud input (ved ikke helt, hvorfor det her skal være der, men det skal det for at wordcloud virker)
n <- 1

# Define UI for application that draws a histogram
ui <- navbarPage("DEN SPROGLIGE VÆRKTØJSKASSE", 
                 theme = shinytheme("flatly"),
                 
                 # Semantik panel
                 tabPanel("Semantisk Model Værktøjer",
                          fluidPage(
                            navlistPanel(
                              "Menu",
                              tabPanel("Semantisk Afstand og Nærmeste Ord",
                                       h3("Semantisk afstand og ordassociationer", 
                                          align = "center"),
                                       textAreaInput(inputId = "textInput1", 
                                                     label = "Indtast et ord", 
                                                     placeholder = "Indtast et ord her..."),
                                       mainPanel(tableOutput("closest_words"),
                                                 tags$style(type="text/css",
                                                            ".shiny-output-error { visibility: hidden; }",
                                                            ".shiny-output-error:before { visibility: hidden; }"),
                                                 )
                                       ),
                              
                              tabPanel("Find ord længst væk",
                                         h3("Den længste semantiske afstand",
                                            align = "center"),
                                       textAreaInput(inputId = "textInput2",
                                                     label = "Indtast et ord",
                                                     placeholder = "Indtast et ord her..."),
                                       mainPanel(tableOutput("antonym"),
                                                 tags$style(type="text/css",
                                                            ".shiny-output-error { visibility: hidden; }",
                                                            ".shiny-output-error:before { visibility: hidden; }"),
                                       )
                                                 ),
                              
                              tabPanel("Visualisering: Et Semantisk Netværk",
                                       h3("Lav et semantisk netværk",
                                          align = "center"),
                                       selectInput(inputId = "model", "Select a model", c("CoNLL17 Model", "The Danish Gigaword Corpus Model")),
                                         mainPanel(plotOutput("selectModel")),
                                       hr(),
                                       conditionalPanel(
                                         condition = "input.model == 'CoNLL17 Model'"
                                       ),
                                       conditionalPanel(
                                         condition = "input.model == 'Danish Gigaword Corpus Model'",
                                       ),
                                       mainPanel(plotOutput("model"))
                                       )
                                       )
                              )
                            ),
                 
                # Word cloud panel
                 tabPanel("Word Cloud",
                          titlePanel("Lav din egen word cloud", windowTitle = "Window title"),
                   sidebarLayout(
                     sidebarPanel(
                       radioButtons(
                         inputId = "source",
                         label = "Lav din egen ordsky",
                         choices = c(
                           "Indtast tekst" = "own",
                           "Upload en tekstfil" = "file"
                         )
                       ),
                       hr(),
                       conditionalPanel(
                         condition = "input.source == 'own'",
                         textAreaInput("text", "Indtast tekst", rows = 7)
                       ),
                       conditionalPanel(
                         condition = "input.source == 'file'",
                         fileInput("file", "Select a file")
                       ),
                       hr(),
                       checkboxInput("remove_words", "Fjern uønskede ord", FALSE),
                       conditionalPanel(
                         condition = "input.remove_words == 1",
                         textAreaInput("words_to_remove1", "Indtast uønskede ord (ét ord per linje)", rows = 1)
                       ),
                       conditionalPanel(
                         condition = "input.remove_words == 1 && input.words_to_remove1.length > 0",
                         textAreaInput("words_to_remove2", "", rows = 1)
                       ),
                       conditionalPanel(
                         condition = "input.remove_words == 1 && input.words_to_remove2.length > 0",
                         textAreaInput("words_to_remove3", "", rows = 1)
                       ),
                       conditionalPanel(
                         condition = "input.remove_words == 1 && input.words_to_remove3.length > 0",
                         textAreaInput("words_to_remove4", "", rows = 1)
                       ),
                       conditionalPanel(
                         condition = "input.remove_words == 1 && input.words_to_remove4.length > 0",
                         textAreaInput("words_to_remove5", "", rows = 1)
                       ),
                       conditionalPanel(
                         condition = "input.remove_words == 1 && input.words_to_remove5.length > 0",
                         textAreaInput("words_to_remove6", "", rows = 1)
                       ),
                       conditionalPanel(
                         condition = "input.remove_words == 1 && input.words_to_remove6.length > 0",
                         textAreaInput("words_to_remove7", "", rows = 1)
                       ),
                       conditionalPanel(
                         condition = "input.remove_words == 1 && input.words_to_remove7.length > 0",
                         textAreaInput("words_to_remove8", "", rows = 1)
                       ),
                       conditionalPanel(
                         condition = "input.remove_words == 1 && input.words_to_remove8.length > 0",
                         textAreaInput("words_to_remove9", "", rows = 1)
                       ),
                       conditionalPanel(
                         condition = "input.remove_words == 1 && input.words_to_remove9.length > 0",
                         textAreaInput("words_to_remove10", "", rows = 1)
                       ),
                       hr(),
                       numericInput("num", "Maksimum antal ord i din wordcloud",
                                    value = 100, min = 5),
                       hr(),
                       colourInput("col", "Baggrundsfarve", value = "white"),
                       hr(),
                     ),
                     mainPanel(
                       wordcloud2Output("cloud"),
                       br(),
                       br()
                     )
                   )
                 ),
     
                 # Sentiment panel
                 tabPanel("Sentiment Analysis",
                          titlePanel("Udregn sentimentværdi for en tekst", windowTitle = "Window title"),
                          sidebarLayout(
                            sidebarPanel(
                              textAreaInput(inputId = "textInput3", label = "Indsæt tekst", placeholder = "Indsæt din tekst her...")
                            ),
                            mainPanel(
                              verbatimTextOutput("value2")
                            ),
                          )
                 ),
                 
                 # Topic Modeling Panel
                 tabPanel("Topic Modeling",
                          titlePanel("Find fremherskende emner i din tekst", windowTitle = "Window title"),
                          sidebarLayout(
                            sidebarPanel(
                              textAreaInput(inputId = "textInput", label = "Indsæt tekst", placeholder = "Indsæt din tekst her...")
                            ),
                            mainPanel(
                            )
                            )
                          ),
                 
                 # "Om" panel
                 tabPanel("Om Projektet",
                          fluidRow(
                            column(6,
                                   "Noget om projektet"),
                            column(3,
                                   img(class="img-polaroid",
                                       src=paste0("http://upload.wikimedia.org/",
                                                  "wikipedia/commons/9/92/",
                                                  "1919_Ford_Model_T_Highboy_Coupe.jpg"))
                            )
                            
                          )
                 )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Semantisk afstand output 
  output$closest_words <- renderTable({
    closest_words <- predict(model_CoNLL17, newdata = input$textInput1, type = "nearest")
    closest_words <- as.data.frame(closest_words)
    names(closest_words)[1] <- "Dit Ord"
    names(closest_words)[2] <- "Nærmeste Ord"
    names(closest_words)[3] <- "Semantisk Lighed"
    names(closest_words)[4] <- "Rangering"
    closest_words
})
  
  # Find ord længst væk output
  output$antonym <- renderTable({
    antonym <- calc_antonym(word = input$textInput2, embedding_matrix = matrix_model_CoNLL17, n = 5)
    antonym <- as.list(antonym)
    antonym <- as.data.frame(antonym)
    antonym
})

  # Semantisk netværk
  model_choice <- renderPlot({
    if (input$model == "CoNLL17 Model") {
      model <- tsne_plot
     } else if (input$model== "The Danish Gigaword Corpus Model") {
        model <- "The Danish Gigaword Corpus Model" 
     }
    return(model)
})
  
  output$model <- renderPlot({
    tsne_plot <- tsne$Y %>%
      as.data.frame() %>%
      mutate(word = row.names(matrix_model_CoNLL17)[2:500]) %>%
      ggplot(aes(x = V1, y = V2, label = word)) + 
      geom_text(size = 3)
    tsne_plot
  })

  #Sentiment analysis output
  output$value2 <- renderText({
    sentida(input$textInput3, output = "total") 
})
  
  # Word cloud output
  data_source <- reactive({
    if (input$source == "own") {
      data <- input$text
    } else if (input$source == "file") {
      data <- input_file()
    }
    
    return(data)
  })
  
  input_file <- reactive({
    if (is.null(input$file)) {
      return("")
    }
    readLines(input$file$datapath, encoding= "UTF-8")
  })
  
  create_wordcloud <- function(data, num_words = 500, background = "white") {
    
    # If text is provided, convert it to a dataframe of word frequencies
    if (is.character(data)) {
      corpus <- Corpus(VectorSource(data))
      corpus <- tm_map(corpus, tolower)
      corpus <- tm_map(corpus, removePunctuation)
      corpus <- tm_map(corpus, removeNumbers)
      corpus <- tm_map(corpus, removeWords, stopwords(tolower("Danish")))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove1))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove2))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove3))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove4))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove5))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove6))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove7))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove8))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove9))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove10))
      tdm <- as.matrix(TermDocumentMatrix(corpus))
      data <- sort(rowSums(tdm), decreasing = TRUE)
      data <- data.frame(word = names(data), freq = as.numeric(data))
    }
    
    # Make sure a proper num_words is provided
    if (!is.numeric(num_words) || num_words < 3) {
      num_words <- 3
    }
    
    # Grab the top n most common words
    data <- head(data, n = num_words)
    if (nrow(data) == 0) {
      return(NULL)
    }
    
    wordcloud2(data, backgroundColor = background, color = "random-dark")
  }
  output$cloud <- renderWordcloud2({
    create_wordcloud(data_source(),
                     num_words = input$num,
                     background = input$col
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)