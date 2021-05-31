
supported_langs <- c("basque", "catalan", "danish", "dutch",
  "en" = "english",
  "esperanto", "finnish", "french", "german", "irish", "italian",
  "latin", "portuguese", "romanian", "somali", "es" = "spanish",
  "sudanese", "swahili", "swedish", "turkish", "vietnamese", "welsh",
  "zulu"
)

#' Get corpus document
#'
#' This function analyzes the news dataframe with the NRC method.
#'
getCorpus <- function(text_v, language) {
  toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))

  txt_new_corpus <- Corpus(VectorSource(text_v))

  # clean and tidy
  txt_new_corpus <- tm_map(txt_new_corpus, toSpace, "/")
  txt_new_corpus <- tm_map(txt_new_corpus, toSpace, "@")
  txt_new_corpus <- tm_map(txt_new_corpus, toSpace, "\\|")
  # Convert the text to lower case
  txt_new_corpus <- tm_map(txt_new_corpus, content_transformer(tolower))
  # Remove numbers
  txt_new_corpus <- tm_map(txt_new_corpus, removeNumbers)
  # Remove english common stopwords
  txt_new_corpus <- tm_map(txt_new_corpus, removeWords, stopwords(language))
  # Remove punctuations
  txt_new_corpus <- tm_map(txt_new_corpus, removePunctuation)
  # Eliminate extra white spaces
  txt_new_corpus <- tm_map(txt_new_corpus, stripWhitespace)

  return(txt_new_corpus)
}

#' Process news with NRC
#'
#' This function analyzes the news dataframe with the NRC method.
#'
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
    char_v <- syuzhet::get_sentences(df[i, target], fix_curly_quotes = TRUE)

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

#' Plot NRC results
#'
#' This function plots the sentiment analysis with NRC.
#'
plotSentiment <- function(df, title, subtitle, translator) {
  caption <- translator$t(
    paste0(
      "Based on package syuzhet (https://cran.r-project.org/web/packages/syuzhet)",
      " and NRC EmoLex (https://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm)"
    )
  )

  custom_theme <- ggplot2::theme(
    strip.background = ggplot2::element_blank(),
    panel.border = element_rect(fill = NA, color = "white"),
    panel.grid.minor = element_blank(),
    panel.grid = ggplot2::element_blank()
  )
  
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
  nrc[nrc$Sentimiento == "anger", ]$Sentimiento <- translator$t("Anger")
  nrc[nrc$Sentimiento == "anticipation", ]$Sentimiento <- translator$t("Anticipation")
  nrc[nrc$Sentimiento == "disgust", ]$Sentimiento <- translator$t("Disgust")
  nrc[nrc$Sentimiento == "fear", ]$Sentimiento <- translator$t("Fear")
  nrc[nrc$Sentimiento == "joy", ]$Sentimiento <- translator$t("Joy")
  nrc[nrc$Sentimiento == "sadness", ]$Sentimiento <- translator$t("Sadness")
  nrc[nrc$Sentimiento == "surprise", ]$Sentimiento <- translator$t("Surprise")

  nrc <- nrc %>% 
    select(Sentimiento, value) %>%
    filter(value != 0) %>% 
    group_by(Sentimiento) %>% 
    summarise(valencia=sum(value))
  
  custom_breaks <- unique(sort(nrc$valencia))
  
  plot <- nrc %>% ggplot2::ggplot(ggplot2::aes(
    x = Sentimiento, y = valencia, fill = Sentimiento # , fill = source.name
  )) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::ggtitle(title, subtitle) +
    ggplot2::labs(caption = caption) +
    ggplot2::coord_flip() +
    ggplot2::xlab(translator$t("Sentiment")) +
    ggplot2::ylab(translator$t("NRC Score")) +
    ggplot2::theme_gray() +
    custom_theme +
    ggplot2::scale_y_continuous(breaks = custom_breaks, limits = c(0, max(custom_breaks))) +
    ggplot2::guides(fill = FALSE) +
    ggplot2::scale_color_brewer()

  return(plot)
}
