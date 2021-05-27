
supported_langs <- c("basque", "catalan", "danish", "dutch", "en" = "english", 
                     "esperanto", "finnish", "french", "german", "irish", "italian", 
                     "latin", "portuguese", "romanian", "somali", "es" = "spanish", 
                     "sudanese", "swahili", "swedish", "turkish", "vietnamese", "welsh", 
                     "zulu")



# ################################# #
processWithNRC <- function(df, lang = "es", target = "content") {
  df$id <- (1:nrow(df))

  language <- "spanish"

  if (lang == "es") {
    language <- "spanish"
  } else if (lang == "en") {
    language <- "english"
  }

  # Collect all descriptions
  for (i in 1:nrow(df)) {
    my_text <- df[i, target]

    char_v <- syuzhet::get_sentences(my_text)

    if (i == 1) {
      nrc_data <- syuzhet::get_nrc_sentiment(char_v, language = language)
      nrc_sum <- colSums(nrc_data)
      nrc_sum <- append(nrc_sum, i)
      names(nrc_sum) <- c(
        "anger", "anticipation", "disgust", "fear", "joy",
        "sadness", "surprise", "trust", "negative",
        "positive", "id"
      )
    } else {
      aux_data <- syuzhet::get_nrc_sentiment(char_v, language = language)
      aux_sum <- colSums(aux_data)
      aux_sum <- append(aux_sum, i)
      names(aux_sum) <- c(
        "anger", "anticipation", "disgust", "fear", "joy",
        "sadness", "surprise", "trust", "negative",
        "positive", "id"
      )

      nrc_sum <- rbind(nrc_sum, aux_sum)
    }
  }

  df_nrc <- as.data.frame(nrc_sum)

  if (!is.array(nrc_sum)) {
    df_nrc <- t(df_nrc)
  }

  rownames(df_nrc) <- 1:nrow(df_nrc)

  df <- df %>%
    dplyr::inner_join(df_nrc, by = c("id" = "id"), copy = TRUE)
  return(df)
}

# ################################# #
plotSentiment <- function(df, title, subtitle) {
  msg <- paste0("Basado en package syuzhet (https://cran.r-project.org/web/packages/syuzhet)",
                " y NRC Emotion Lexicon (https://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm)")

  nrc <-
    tidyr::pivot_longer(
      df,
      cols = c(
        "anger",
        "anticipation",
        "disgust",
        "fear",
        "joy",
        "sadness",
        "surprise",
        "trust",
        "negative",
        "positive"
      ),
      names_to = "Sentimiento"
    ) %>% dplyr::filter(!(Sentimiento %in% c("positive", "negative", "trust")))
  nrc
  nrc[nrc$Sentimiento == "anger", ]$Sentimiento <- "Ira"
  nrc[nrc$Sentimiento == "anticipation", ]$Sentimiento <- "Expectativa"
  nrc[nrc$Sentimiento == "disgust", ]$Sentimiento <- "Desagrado"
  nrc[nrc$Sentimiento == "fear", ]$Sentimiento <- "Miedo"
  nrc[nrc$Sentimiento == "joy", ]$Sentimiento <- "Alegría"
  nrc[nrc$Sentimiento == "sadness", ]$Sentimiento <- "Tristeza"
  nrc[nrc$Sentimiento == "surprise", ]$Sentimiento <- "Sorpresa"

  plot <- nrc %>% ggplot2::ggplot(ggplot2::aes(
    x = Sentimiento, y = value, fill = Sentimiento # , fill = source.name
  )) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::ggtitle(title, subtitle) +
    ggplot2::labs(caption = msg) +
    ggplot2::coord_flip() +
    ggplot2::xlab("Sentimiento") +
    ggplot2::ylab("Puntaje NRC") +
    ggplot2::theme_gray() +
    ggplot2::guides(fill = FALSE) +
    ggplot2::scale_color_brewer()

  return(plot)
}
