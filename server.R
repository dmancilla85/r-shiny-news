source("./R/newsapi.R", local = TRUE, encoding = c("UTF-8"))
source("./R/sentiment.R", local = TRUE, encoding = c("UTF-8"))

# Define server logic
server <- function(input, output, session) {
  output$dt_fechas <- shiny::renderUI({
    shiny::dateRangeInput(
      inputId = "dt_fechas",
      label = "Rango de fechas:",
      min = Sys.Date() - 3,
      start = Sys.Date() - 3,
      max = Sys.Date(),
      end = Sys.Date(),
      format = "dd/mm/yy",
      language = input$sel_language,
      separator = " - "
    )
  })

  # TODO: revisar validez del analisis de sentimiento
  values <- shiny::reactiveValues()
  values$is_empty <- TRUE

  shiny::observeEvent(input$btn_start, {
    values$date_range <- shiny::isolate(input$dt_fechas)
    values$caption_txt <- shiny::isolate(input$txt_caption)
    values$country <- shiny::isolate(input$sel_country)
    values$lang <- shiny::isolate(input$sel_language)
    values$category <- shiny::isolate(input$sel_category)
    values$top_news <- shiny::isolate(input$chk_latest_news)
    values$titles <- shiny::isolate(input$chk_search_titles)


    if (!is.null(values$date_range) &
      (!is.null(values$caption_txt) & stringr::str_length(values$caption_txt) > 3)) {
      newsApi <- NewsApi(
        # Language
        p_from = values$date_range[1],
        # older as one month back (free User)
        p_to = values$date_range[2],
        p_country = values$country,
        p_language = values$lang,
        # dates surround with "
        p_query = values$caption_txt,
        # sort criteria
        p_topNews = values$top_news,
        p_category = values$category,
        p_searchInTitles = values$titles
      )

      values$df_req <- getNews(newsApi)

      if (is.null(values$df_req) || nrow(values$df_req) == 0) {
        values$is_empty <- TRUE
        js_string <- 'alert("No hay resultados");'
        session$sendCustomMessage(type = "jsCode", list(
          value = js_string
        ))
      } else {
        values$is_empty <- FALSE
      }
    }
  })

  output$plt_sentiment <- shiny::renderPlot({
    title <- stringr::str_to_title(
      stringr::str_interp("Palabra buscada: ${values$caption_txt}")
    )
    subtitle <- stringr::str_interp("Análisis de sentimiento NRC (Emoción - Lexicon)")

    if (values$is_empty) {
      ggplot2::ggplot() +
        ggplot2::geom_blank()
    }
    else {
      # show plot
      
      
      nrc <- processWithNRC(values$df_req, values$lang)
      
      View(head(nrc))
      
      plotSentiment(nrc, title = title, subtitle = subtitle)
    }
  })

  output$tbl_sentiment <- DT::renderDataTable({
    if (values$is_empty) {
      DT::datatable(NULL)
    } else {
      df_formatted <- values$df_req
      df_formatted$Imagen <- paste0(
        "<a href='", df_formatted$url, "'>",
        "<img src=", df_formatted$urlToImage,
        " height=64></img></a>"
      )
      df_formatted$urlToImage <- NULL

      df_formatted %>%
        select("Título" = title, Imagen, "Fuente" = source.name, "Descripción" = description) %>%
        DT::datatable(
          escape = FALSE,
          rownames = FALSE,
          class = "compact stripe",
          style = "bootstrap",
          selection = "none",
          options = list(
            dom = "tip",
            language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json"),
            initComplete = DT::JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff', 'font-size':'11px'});",
              "$(this.api().table().body()).css({'background-color': '#C6B3B9', 'color': '#000', 'font-size':'10px'});",
              "}"
            )
          )
        )
    }
  })
}
