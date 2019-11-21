#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    singleton(tags$head(tags$script(
        HTML(
            'Shiny.addCustomMessageHandler("jsCode",function(message) {eval(message.value);});'
        )
    ))),
    
    titlePanel("Análisis de Noticias..."),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput(
                "bins",
                "Number of bins:",
                min = 1,
                max = 50,
                value = 30
            ),
            
            selectInput(
                inputId = "country",
                label = "País",
                choices = c(
                    "Argentina" = "ar",
                    "Chile" = "cl",
                    "Brasil" = "br",
                    "Paraguay" = "py"
                )
            ),
            
            selectInput(
                inputId = "language",
                label = "Idioma",
                choices = c("Español" = "es",
                            "Inglés" = "en") ,
                selected = "es"
            ),
            checkboxInput(inputId = "onlyLatestNews", "Sólo últimas noticias"),
            conditionalPanel(
                condition = "input.onlyLatestNews == true",
                selectInput(
                    inputId = "category",
                    label = "Categoría",
                    choices = c(
                        "General" = "general",
                        "Negocios" = "business",
                        "Entretenimiento" = "entertainment",
                        "Salud" = "health",
                        "Ciencia" = "science",
                        "Deportes" = "sports",
                        "Tecnología" = "technology"
                    )
                )
            ),
            conditionalPanel(
                condition = "input.onlyLatestNews == false",
                uiOutput("daterange"),
                textInput(
                    inputId = "caption",
                    label = "Frase a buscar",
                    placeholder = "Dólar"
                ),
                verbatimTextOutput("validation")
            ),
            actionButton("start", "Iniciar")
            
        ),
        
        # Show a plot of the generated distribution
        mainPanel(plotOutput("distPlot"))
    ),
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    observeEvent(input$start, {
        js_string <- 'alert("Thank you for clicking");'
        session$sendCustomMessage(type = 'jsCode', list(value = js_string))
    })
    
    output$validation <- reactive({
        validate(
            need(
                input$caption != "" &&
                    input$onlyLatestNews == FALSE,
                "Ingrese una frase a buscar"
            ),
            need(
                input$daterange[1] < input$daterange[2],
                "El rango de fechas es incorrecto"
            )
        )
        
    })
    
    output$daterange <- renderUI({
        dateRangeInput(
            "daterange",
            "Rango de fechas:",
            min    = Sys.Date() - 30,
            start  = Sys.Date() - 30,
            max    = Sys.Date(),
            end    = Sys.Date(),
            format = "dd/mm/yy",
            language = input$language,
            separator = " - "
        )
        
    })
    
    
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        # draw the histogram with the specified number of bins
        hist(x,
             breaks = bins,
             col = 'darkgray',
             border = 'white')
        
    })
}

# Run the application
shinyApp(ui = ui, server = server)
