
supported_langs <- c(
  "basque",
  "catalan",
  "no" = "danish",
  "nl" = "dutch",
  "en" = "english",
  "esperanto",
  "finnish",
  "fr" = "french",
  "de" = "german",
  "irish",
  "it" = "italian",
  "latin",
  "pt" = "portuguese",
  "romanian",
  "somali",
  "es" = "spanish",
  "sudanese",
  "swahili",
  "swedish",
  "turkish",
  "vietnamese",
  "welsh",
  "zulu"
)



#' Get corpus document
#'
#' This function analyzes the news dataframe with the NRC method.
#'
getCorpus <- function(text_v, language) {
  txt_new_corpus <- tm::Corpus(tm::VectorSource(text_v))
  toSpace <- tm::content_transformer(function(x, pattern) gsub(pattern, " ", x))
  
  # clean and tidy
  txt_new_corpus <- tm::tm_map(txt_new_corpus, toSpace, "/")
  txt_new_corpus <- tm::tm_map(txt_new_corpus, toSpace, "\n")
  txt_new_corpus <- tm::tm_map(txt_new_corpus, toSpace, "\r")
  txt_new_corpus <- tm::tm_map(txt_new_corpus, toSpace, "@")
  txt_new_corpus <- tm::tm_map(txt_new_corpus, toSpace, "\\|")
  # Convert the text to lower case
  txt_new_corpus <- tm::tm_map(txt_new_corpus, tm::content_transformer(tolower))
  # Remove numbers
  txt_new_corpus <- tm::tm_map(txt_new_corpus, tm::removeNumbers)
  # Remove english common stopwords
  txt_new_corpus <- tm::tm_map(txt_new_corpus, tm::removeWords, tm::stopwords(language))
  # Remove punctuations
  txt_new_corpus <- tm::tm_map(txt_new_corpus, tm::removePunctuation)
  # Eliminate extra white spaces
  txt_new_corpus <- tm::tm_map(txt_new_corpus, tm::stripWhitespace)

  return(txt_new_corpus)
}

#' Process news with NRC
#'
#' This function analyzes the news dataframe with the NRC method.
#'
processWithNRC <- function(df, lang = "es", target = "content") {
  df$id <- (1:nrow(df))

  language <- supported_langs[lang]

  df <- df %>% dplyr::mutate(content = stringr::str_replace(content, "/", " "))
  df <- df %>% dplyr::mutate(content = stringr::str_replace(content, "\n", " "))
  df <- df %>% dplyr::mutate(content = stringr::str_replace(content, "\r", " "))
  df <- df %>% dplyr::mutate(content = stringr::str_replace(content, "@", " "))
  df <- df %>% dplyr::mutate(content = stringr::str_replace(content, "\\|", " "))
  df <- df %>% dplyr::mutate(content = stringr::str_replace(content, "...", " "))
  df$content <- gsub('[0-9.]', '', df$content)

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