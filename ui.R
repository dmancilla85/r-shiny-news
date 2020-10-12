library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  singleton(tags$head(tags$script(
    HTML(
      'Shiny.addCustomMessageHandler("jsCode",function(message) {eval(message.value);});'
    )
  ))),
  titlePanel(title = HTML("<b>ANÁLISIS DE SENTIMIENTO EN TITULARES DE NOTICIAS (Enfoque basado en EmoLex)</b>")),


  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "sel_language",
        label = "Idioma",
        choices = c(
          "Español" = "es",
          "Inglés" = "en"
        ),
        selected = "es"
      ),
      checkboxInput(
        inputId = "chk_latest_news", label = "Sólo últimas noticias",
        value = TRUE
      ),
      conditionalPanel(
        condition = "input.chk_latest_news == true",
        selectInput(
          inputId = "sel_country",
          label = "País",
          selected = "ar",
          choices = c(
            "Argentina" = "ar",
            "Chile" = "cl",
            "Brasil" = "br",
            "Paraguay" = "py",
            "Estados Unidos" = "us"
          )
        ),
        selectInput(
          inputId = "sel_category",
          label = "Categoría",
          selected = "*",
          choices = c(
            "Todas" = "*",
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
        condition = "input.chk_latest_news == false",
        uiOutput(outputId = "dt_fechas"),
        checkboxInput(
          inputId = "chk_search_titles",
          label = "Buscar en títulos",
          value = TRUE
        )
      ),
      textInput(
        inputId = "txt_caption",
        label = "Frase a buscar",
        value = NULL,
        placeholder = "Ingrese una palabra específica"
      ),
      actionButton(inputId = "btn_start", label = "Buscar")
    ),

    # Show a plot of the generated distribution
    mainPanel(plotOutput("pl_sentiment"))
  ),
)
