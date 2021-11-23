
#' Line breaks generator
#'
#' This function generates n line breaks.
#'
linebreaks <- function(n) {
  htmltools::HTML(strrep(htmltools::br(), n))
}

# Define UI for application that draws a histogram
ui <- shiny::fluidPage(
  shiny.i18n::usei18n(i18n),
  # Somewhere in UI
  add_busy_spinner(spin = "breeding-rhombus", margins = c(30, 30), color = "lightblue"),
  theme = "custom.css",
  # Application title
  htmltools::singleton(htmltools::tags$head(htmltools::tags$script(
    htmltools::HTML(
      'Shiny.addCustomMessageHandler("jsCode",function(message) {eval(message.value);});'
    )
  ))),
  htmltools::tags$head(
    htmltools::tags$link(rel = "preconnect", href = "https://fonts.gstatic.com"),
    htmltools::tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Merienda&display=swap")
  ),
  titlePanel(
    title = htmltools::HTML(stringr::str_interp('<b>${i18n$t("Sentiment analysis on news")}</b>')),
    windowTitle = "Sentiment Analysis"
  ),
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
