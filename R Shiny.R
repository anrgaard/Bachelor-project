# Packages
library(pacman)
p_load(shiny, shinydashboard, shinythemes, tm, SnowballC, wordcloud, RColorBrewer, readr, colourpicker, wordcloud2)

# Sentida
#if(!require("devtools")) install.packages("devtools")
#devtools::install_github("Guscode/Sentida")
library(Sentida)

# Wordcloud2
#require(devtools)
#install_github("lchiffon/wordcloud2")

# Wordcloud input (ved ikke helt, hvorfor det her skal være der, men det skal det for at virke)
n <- 1

# Define UI for application that draws a histogram
ui <- navbarPage("DEN SPROGLIGE VÆRKTØJSKASSE", 
                 theme = shinytheme("flatly"),
                 
                 # Semantik panel
                 tabPanel("Semantiske Afstande",
                          titlePanel("Find Semantiske Afstande", windowTitle = "Window title"),
                          sidebarLayout(
                            sidebarPanel(
                              textAreaInput(inputId = "textInput1", label = "Indtast et ord", placeholder = "Indtast et ord her..."),
                            ),
                            mainPanel(
                              tableOutput("value1")
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
  output$value1 <- renderUI({
    closest_words <- predict(model_CoNLL17, newdata = input$textInput1, type = "nearest")
    closest_words <- as.data.frame(closest_words)
    closest_words$textInput1.term2})
  
  #Sentiment analysis output
  output$value2 <- renderText({
    sentida(input$textInput3, output = "total") })
  
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
    readLines(input$file$datapath)
  })
  
  create_wordcloud <- function(data, num_words = 100, background = "white") {
    
    # If text is provided, convert it to a dataframe of word frequencies
    if (is.character(data)) {
      corpus <- Corpus(VectorSource(data))
      corpus <- tm_map(corpus, tolower)
      corpus <- tm_map(corpus, removePunctuation)
      corpus <- tm_map(corpus, removeNumbers)
      corpus <- tm_map(corpus, removeWords, stopwords(tolower(input$language)))
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
    
    wordcloud2(data, backgroundColor = background)
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