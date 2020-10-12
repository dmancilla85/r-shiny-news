source("./src/newsapi.R", local = TRUE, encoding = c("UTF-8"))
source("./src/sentiment.R", local = TRUE, encoding = c("UTF-8"))

# Define server logic
server <- function(input, output, session) {
  output$dt_fechas <- renderUI({
    dateRangeInput(
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
  
  output$pl_sentiment <- renderPlot({
    input$btn_start

    date_range <- isolate(input$dt_fechas)
    caption_txt <- isolate(input$txt_caption)
    country <- isolate(input$sel_country)
    lang <- isolate(input$sel_language)
    category <- isolate(input$sel_category)
    top_news <- isolate(input$chk_latest_news)
    titles <- isolate(input$chk_search_titles)

    if (is.null(caption_txt) | caption_txt == "" ) {
      #js_string <- 'alert("Ingrese algo para buscar");'
      #session$sendCustomMessage(type = "jsCode", list(value = js_string))
      ggplot() +
        geom_blank()
    }

    if (!is.null(date_range) &
      (!is.null(caption_txt) & str_length(caption_txt) > 3)) {
      newsApi <- NewsApi(
        # Language
        p_from = date_range[1],
        # older as one month back (free User)
        p_to = date_range[2],
        p_country = country,
        p_language = lang,
        # dates surround with "
        p_query = caption_txt,
        # sort criteria
        p_topNews = top_news,
        p_category = category,
        p_searchInTitles = titles
      )

      df_req <- getNews(newsApi)

      if (is.null(dim(df_req)) || dim(df_req)[1] == 0) {
        js_string <- 'alert("No hay resultados");'
        session$sendCustomMessage(type = "jsCode", list(value = js_string))
        ggplot() +
          geom_blank()
      }
      else {
        # show plot
        nrc <- processWithNRC(df_req, lang)
        plotSentiment(nrc, caption_txt, "Subtitulo")
      }
    }
  })
}
