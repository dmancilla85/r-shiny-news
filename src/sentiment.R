# ################################# #
processWithNRC <- function(df, lang = "es") {
  df$id <- (1:nrow(df))

  language <- "spanish"

  if (lang == "es") {
    language <- "spanish"
  } else if (lang == "en") {
    language <- "english"
  }

  # Collect all descriptions
  for (i in 1:nrow(df)) {
    my_text <- df$content[i]
    char_v <- get_sentences(my_text)

    if (i == 1) {
      nrc_data <- get_nrc_sentiment(char_v, language = language)
      nrc_sum <- colSums(nrc_data)
      nrc_sum <- append(nrc_sum, i)
      names(nrc_sum) <- c(
        "anger", "anticipation", "disgust", "fear", "joy",
        "sadness", "surprise", "trust", "negative",
        "positive", "id"
      )
    } else {
      aux_data <- get_nrc_sentiment(char_v, language = language)
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

  View(df)
  View(nrc_sum)
  df <- df %>% inner_join(as.data.frame(nrc_sum), by = c("id" = "id"))

  return(df)
}

# ################################# #
plotSentiment <- function(df, title, subtitle) {
  nrc <-
    pivot_longer(
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
    ) %>% filter(!(Sentimiento %in% c("positive", "negative", "trust")))
  nrc
  nrc[nrc$Sentimiento == "anger", ]$Sentimiento <- "Ira"
  nrc[nrc$Sentimiento == "anticipation", ]$Sentimiento <- "Sorpresa"
  nrc[nrc$Sentimiento == "disgust", ]$Sentimiento <- "Desagrado"
  nrc[nrc$Sentimiento == "fear", ]$Sentimiento <- "Miedo"
  nrc[nrc$Sentimiento == "joy", ]$Sentimiento <- "Alegría"
  nrc[nrc$Sentimiento == "sadness", ]$Sentimiento <- "Tristeza"
  nrc[nrc$Sentimiento == "surprise", ]$Sentimiento <- "Sorpresa"

  plot <- nrc %>% ggplot(aes(x = Sentimiento, y = value, fill = source.name)) +
    geom_bar(stat = "identity", col = "black") +
    ggtitle(title, subtitle) +
    coord_flip() +
    xlab("Sentimiento") +
    ylab("Puntaje") +
    labs(fill = "Fuente") +
    scale_color_brewer()

  return(plot)
}
