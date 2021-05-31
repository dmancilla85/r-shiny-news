
mySidebarPanel <- # Sidebar with a slider input for number of bins
  function() {
    shiny::sidebarPanel(
      shiny::selectInput(
        inputId = "sel_language",
        label = i18n$t("Language"),
        # choices = i18n$get_languages(),
        selected = i18n$get_key_translation(),
        choices = c(
          "Español" = "es",
          "English" = "en",
          "Arab" = "ar",
          "Deutsche" = "de"
        ),
      ),
      checkboxInput(
        inputId = "chk_latest_news", label = i18n$t("Top headlines"),
        value = TRUE
      ),
      shiny::conditionalPanel(
        condition = "input.chk_latest_news == true",
        selectInput(
          inputId = "sel_country",
          label = i18n$t("Country"),
          selected = "ar",
          choices = c(
            "Argentina" = "ar",
            "Chile" = "cl",
            "Brasil" = "br",
            "Paraguay" = "py",
            "United States" = "us"
          )
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
          %>% stats::setNames(nm = c(
              i18n$t("All")$children[[1]],
              i18n$t("General")$children[[1]],
              i18n$t("Business")$children[[1]],
              i18n$t("Entertainment")$children[[1]],
              i18n$t("Health")$children[[1]],
              i18n$t("Science")$children[[1]],
              i18n$t("Sports")$children[[1]],
              i18n$t("Technology")$children[[1]]
            ))
        )
      ),
      shiny::conditionalPanel(
        condition = "input.chk_latest_news == false",
        uiOutput(outputId = "dt_fechas"),
        checkboxInput(
          inputId = "chk_search_titles",
          label = i18n$t("Search in headlines"),
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
        tags$div(i18n$t("Loading..."), id = "loadmessage")
      )
    )
  }
