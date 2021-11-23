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
    panel.border = ggplot2::element_rect(fill = NA, color = "white"),
    panel.grid.minor = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(size = 16, family = "Calibri"),
    plot.subtitle = ggplot2::element_text(size = 14, color = "darkcyan"),
    axis.text = ggplot2::element_text(size = 12),
    axis.title = ggplot2::element_text(size = 14, color = "darkcyan", face = "italic"),
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
    dplyr::select(Sentimiento, value) %>%
    dplyr::filter(value != 0) %>%
    dplyr::group_by(Sentimiento) %>%
    dplyr::summarise(valencia = sum(value))

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
