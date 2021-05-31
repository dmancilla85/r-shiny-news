
# Define server logic
server <- function(input, output, session) {

  # internationalization
  observeEvent(input$sel_language, {
    shiny.i18n::update_lang(session, input$sel_language)
  })

  output$dt_fechas <- shiny::renderUI({
    shiny::dateRangeInput(
      inputId = "dt_fechas",
      label = i18n$t("Date range:"),
      min = Sys.Date() - 3,
      start = Sys.Date() - 3,
      max = Sys.Date(),
      end = Sys.Date(),
      format = i18n$t("mm/dd/yy"),
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
    values$all_langs <- shiny::isolate(input$chk_all_languages)


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
        empty_msg <- i18n$t("No results")

        showModal(modalDialog(
          shiny::renderText(empty_msg),
          easyClose = TRUE,
          footer = NULL,
          size = "m"
        ))
      } else {
        values$is_empty <- FALSE
      }
    }
  })

  output$plt_sentiment <- shiny::renderPlot({
    title <- stringr::str_to_title(
      stringr::str_interp(str_interp("${i18n$t('Searched word')}: ${values$caption_txt}"))
    )
    subtitle <- i18n$t("NRC Sentiment Analysis (Emotion-Lexicon)")

    if (values$is_empty) {
      ggplot2::ggplot() +
        ggplot2::geom_blank()
    }
    else {
      # show plot
      nrc <- processWithNRC(values$df_req, values$lang)

      plotSentiment(nrc, title = title, subtitle = subtitle, translator = i18n)
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
        select("Title" = title, "Image" = Imagen, "Source" = source.name, "Description" = description) %>%
        rename_all(i18n$t) %>%
        DT::datatable(
          escape = FALSE,
          rownames = FALSE,
          class = "compact stripe",
          style = "bootstrap",
          selection = "none",
          options = list(
            dom = "tip",
            language = list(
              url =
                stringr::str_interp("//cdn.datatables.net/plug-ins/1.10.11/i18n/${i18n$t('English.json')}")
            ),
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
