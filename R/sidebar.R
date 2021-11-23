
#' Get available countries
#'
#' Return the list of countries
#'
getAvailableCountries <- function() {
  countries <- read.table("./data/countries",
    header = TRUE,
    sep = ",", encoding = "UTF-8"
  )
  api_countries <- read.table("./data/api_countries",
    header = TRUE,
    sep = ",", encoding = "UTF-8"
  )
  countries$Code <- tolower(countries$Code)

  filtered <- countries %>%
    dplyr::inner_join(api_countries, by = "Code")

  countries <- filtered$Code
  names(countries) <- filtered$Name
  return(countries)
}

mySidebarPanel <- # Sidebar with a slider input for number of bins
  function() {
    shiny::sidebarPanel(
      shiny::selectInput(
        inputId = "sel_language",
        label = i18n$t("Language"),
        selected = i18n$get_key_translation(),
        choices = c(
          # "Arab" = "ar",
          "Deutsche" = "de",
          "Español" = "es",
          "English" = "en",
          "Françoise" = "fr",
          "Portuguese" = "pt",
          "Italiano" = "it",
          "Norsk" = "no",
          "Dutch" = "nl"
        ),
      ),
      checkboxInput(
        inputId = "chk_latest_news", label = i18n$t("Top headlines"),
        value = TRUE
      ),
      shiny::conditionalPanel(
        condition = "input.chk_latest_news == true",
        shiny::selectInput(
          inputId = "sel_country",
          label = i18n$t("Country"),
          selected = "ar",
          choices = getAvailableCountries()
        ),
        shiny::selectInput(
          inputId = "sel_category",
          label = i18n$t("Category"),
          selected = "*",
          selectize = T,
          choices = c(
            "All" = "*",
            "General" = "general",
            "Business" = "business",
            "Entertainment" = "entertainment",
            "Health" = "health",
            "Science" = "science",
            "Sports" = "sports",
            "Technology" = "technology"
          )
        )
      ),
      shiny::conditionalPanel(
        condition = "input.chk_latest_news == false",
        shiny::uiOutput(outputId = "dt_fechas"),
        shiny::checkboxInput(
          inputId = "chk_search_titles",
          label = i18n$t("Only analize headlines"),
          value = FALSE
        )
      ),
      shiny::textInput(
        inputId = "txt_caption",
        label = i18n$t("Word to search"),
        value = NULL,
        placeholder = ""
      ),
      shiny::actionButton(inputId = "btn_start", label = i18n$t("Search")),
      shiny::conditionalPanel(
        condition = "$('html').hasClass('shiny-busy')",
        htmltools::tags$div(i18n$t("Loading..."), id = "loadmessage")
      )
    )
  }
