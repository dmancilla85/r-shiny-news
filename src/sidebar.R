
mySidebarPanel <- # Sidebar with a slider input for number of bins
  function() {
    shiny::sidebarPanel(
      shiny::selectInput(
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
      shiny::conditionalPanel(
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
        shiny::selectInput(
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
      shiny::conditionalPanel(
        condition = "input.chk_latest_news == false",
        uiOutput(outputId = "dt_fechas"),
        checkboxInput(
          inputId = "chk_search_titles",
          label = "Buscar en títulos",
          value = TRUE
        )
      ),
      shiny::textInput(
        inputId = "txt_caption",
        label = "Frase a buscar",
        value = NULL,
        placeholder = "Ingrese una palabra específica"
      ),
      shiny::actionButton(inputId = "btn_start", label = "Buscar"),
      shiny::conditionalPanel(
        condition = "$('html').hasClass('shiny-busy')",
        tags$div("Cargando...", id = "loadmessage")
      )
    )
  }
