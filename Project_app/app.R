library(shiny)
library(Sentida)
library(shinythemes)


# Define UI for application that draws a histogram
ui <- navbarPage("Bachelor Appzz!", 
                 theme = shinytheme("cerulean"),
                 
                 # Semantik panel
                 tabPanel("Semantiske afstande",
                          titlePanel("Semantiske afstande", windowTitle = "Window title")),
                 
                 # Word cloud panel
                 tabPanel("Word cloud generator",
                          titlePanel("Word cloud generator", windowTitle = "Window title")),
                 # Sentiment panel
                 tabPanel("Sentiment Analyse",
                          titlePanel("Sentiment analyse", windowTitle = "Window title"),
                          sidebarLayout(
                              sidebarPanel(
                                  textAreaInput(inputId = "textInput", label = "Din tekst", placeholder = "Indsæt din tekst her...")
                              ),
                              mainPanel(
                                  verbatimTextOutput("value")
                              ),
                          )
                 ),
                 
                 # "Om" panel
                 tabPanel("Om projektet",
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
    output$value <- renderText({
        sentida(input$textInput) })
}

# Run the application 
shinyApp(ui = ui, server = server)
