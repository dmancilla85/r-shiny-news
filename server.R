source("./src/newsapi.R", local = TRUE, encoding = c("UTF-8"))
source("./src/sentiment.R", local = TRUE, encoding = c("UTF-8"))

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
    }
  })

  output$plt_sentiment <- shiny::renderPlot({
    title <- stringr::str_to_title(values$caption_txt)
    subtitle <- stringr::str_interp("Artículos que contienen la palabra ${title}")

    if (is.null(values$df_req)) {
      ggplot2::ggplot() +
        ggplot2::geom_blank()
    }
    else {
      if (dim(values$df_req)[1] == 0) {
        js_string <- 'alert("No hay resultados");'
        session$sendCustomMessage(type = "jsCode", list(value = js_string))
        ggplot2::ggplot() +
          ggplot2::geom_blank()
      }
      else {
        # show plot
        nrc <- processWithNRC(values$df_req, values$lang)
        plotSentiment(nrc, title = title, subtitle = subtitle)
      }
    }
  })

  output$tbl_sentiment <- renderDataTable({
    input$btn_start
  })
}
