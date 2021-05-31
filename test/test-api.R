source("./R/newsapi.R", local = TRUE, encoding = c("UTF-8"))
source("./R/sentiment.R", local = TRUE, encoding = c("UTF-8"))
library(dplyr)
library(future)

obj <- NewsApi(
  # Language
  p_from = NULL,
  # older as one month back (free User)
  p_to = NULL,
  p_country = "ar",
  p_language = "es",
  # dates surround with "
  p_query = "covid",
  # sort criteria
  p_topNews = TRUE,
  p_category = "general",
  p_searchInTitles = FALSE
)

df <- getTopHeadLines(obj)


ep_top_headlines <- "https://newsapi.org/v2/top-headlines"

auch <- api(ep_top_headlines) %>%
  api_query(
    country = "ar",
    apiKey = "f4a88e83555a4ffd91669835d38efedf",
    category = "general",
    pageSize = 10,
    language = "es"
  ) %>%
  api_error_handler(warn_for_status)





resp <- GET(
  url = ep_top_headlines,
  query = list(
    country = "ar",
    apiKey = "f4a88e83555a4ffd91669835d38efedf",
    category = "general",
    pageSize = 10,
    language = "es"
  )
)

if (http_type(resp) != "application/json") {
  stop("API did not return json", call. = FALSE)
} else {
  print(http_type(resp))
}

lista <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)

df <- convertFromJSON(lista)

nrc <- processWithNRC(df)

# TODO: Graficar aporte positivo/negativo de cada fuente
aux <- pivot_longer(nrc, c("positive", "negative"), names_to = "valence", values_to = "valor")
aux %>% select(source.name, valence, valor)
