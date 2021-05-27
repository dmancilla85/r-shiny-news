source("./R/sidebar.R", local = TRUE, encoding = c("UTF-8"))


linebreaks <- function(n) {
  HTML(strrep(br(), n))
}

# Define UI for application that draws a histogram
ui <- shiny::fluidPage(
  theme = "custom.css",
  # Application title
  singleton(tags$head(tags$script(
    HTML(
      'Shiny.addCustomMessageHandler("jsCode",function(message) {eval(message.value);});'
    )
  ))),
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com"),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Merienda&display=swap")
  ),
  titlePanel(title = HTML("<b>Análisis de sentimiento en noticias</b>"), windowTitle = "Análisis de sentimiento"),
  linebreaks(1),

  # Sidebar with a slider input for number of bins
  shiny::sidebarLayout(
    sidebarPanel = mySidebarPanel(),

    # Show a plot of the generated distribution
    mainPanel = shiny::mainPanel(
      fluidRow(
        shinydashboard::box(
          width = 12,
          shinycustomloader::withLoader(plotOutput(outputId = "plt_sentiment"))
        ),
        shinydashboard::box(
          title = "",
          width = 12,
          shinycustomloader::withLoader(DT::dataTableOutput(outputId = "tbl_sentiment"))
        )
      )
    )
  )
)
