#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("framework.r")

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    # singleton(tags$head(tags$script(
    #     HTML(
    #         'Shiny.addCustomMessageHandler("jsCode",function(message) {eval(message.value);});'
    #     )
    # ))),
    
    titlePanel("Análisis de sentimiento de noticias (Enfoque basado en EmoLex)"),
    
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
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
                    inputId = "country",
                    label = "País",
                    selected = "ar",
                    choices = c(
                        "Argentina" = "ar",
                        "Chile" = "cl",
                        "Brasil" = "br",
                        "Paraguay" = "py"
                    )
                ),
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
                uiOutput("daterange")
            ),
            textInput(
                inputId = "caption",
                label = "Frase a buscar",
                value = "Covid-19",
                placeholder = "Ingrese una palabra específica"
            ),
            verbatimTextOutput("validation"),
            actionButton("start", "Iniciar")
            
        ),
        
        # Show a plot of the generated distribution
        mainPanel(plotOutput("distPlot"))
    ),
)


# Define server logic
server <- function(input, output, session) {
    observeEvent(input$start, {
        js_string <- 'alert("Thank you for clicking");'
        session$sendCustomMessage(type = 'jsCode', list(value = js_string))
    })
    
    # output$validation <- reactive({
    #     validate(
    #         need( !is.null(input) & input$caption != "" ,
    #             "Ingrese una frase a buscar"
    #         ),
    #         need(str_length(input) > 4 ,
    #              "La longitud debe ser mayor que 4"
    #         )
    #     )
    #
    # })
    
    output$daterange <- renderUI({
        dateRangeInput(
            "daterange",
            "Rango de fechas:",
            min    = Sys.Date() - 3,
            start  = Sys.Date() - 3,
            max    = Sys.Date(),
            end    = Sys.Date(),
            format = "dd/mm/yy",
            language = input$language,
            separator = " - "
        )
        
    })
    
    
    output$distPlot <- renderPlot({
        if (!is.null(input$daterange) && (!is.null(input$caption)
                                          &&
                                          str_length(input$caption) > 3))
        {
            newsApi <- NewsApi(
                # Language
                p_from = input$daterange[1],
                # older as one month back (free User)
                p_to = input$daterange[2],
                p_country = input$country,
                p_language = input$language,
                # dates surround with "
                p_query = input$caption,
                # sort criteria
                p_topNews = input$onlyLatestNews,
                p_category = input$category,
                p_searchInTitles = TRUE
            )
            
            df_req <- getNews(newsApi)
            
            if (is.null(dim(df_req)) || dim(df_req)[1] == 0)
            {
                # show nothing
                print("nothing here...")
                
                ggplot() + geom_blank()
                
            }
            else
            {
                # show plot
                nrc <- processWithNRC(df_req, input$language)
                
                plotSentiment(nrc, input$caption, "Subtitulo")
                
            }
        }
        
        
    })
}

# Run the application
shinyApp(ui = ui, server = server)
