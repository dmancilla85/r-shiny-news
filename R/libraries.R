
#' Load all required libraries
#'
#' This function installs or loads all required libraries.
#'
loadAllLibraries <- function() {
  print("Loading libraries...")

  if (!require(extrafont)) {
    install.packages("extrafont")
    library(extrafont)
  }
  
  if (!require(shinybusy)) {
    install.packages("shinybusy")
    library(shinybusy)
  }

  if (!require(tm)) {
    install.packages("tm")
    library(tm)
  }

  if (!require(shiny.i18n)) {
    install.packages("shiny.i18n")
    library(shiny.i18n)
  }

  if (!require(modules)) {
    install.packages("modules")
    library(modules)
  }

  # Sentiment Analysis
  if (!require(syuzhet)) {
    install.packages("syuzhet")
    library(syuzhet)
  }

  # Shiny framework
  if (!require(shiny)) {
    install.packages("shiny")
    library(shiny)
  }

  # Sentiment Analysis
  if (!require(shinycustomloader)) {
    install.packages("shinycustomloader")
    library(shinycustomloader)
  }

  # String manipulations
  if (!require(stringr)) {
    install.packages("stringr")
    library(stringr)
  }

  # Handling promises
  if (!require(future)) {
    install.packages("future")
    library(future)
  }

  # Data  manipulations
  if (!require(dplyr)) {
    install.packages("dplyr")
    library(dplyr)
  }

  if (!require(tidyr)) {
    install.packages("tidyr")
    library(tidyr)
  }

  # Data visualizations
  if (!require(ggplot2)) {
    install.packages("ggplot2")
    library(ggplot2)
  }

  # JSON format
  if (!require(jsonlite)) {
    install.packages("jsonlite")
    library(jsonlite)
  }

  # API connections
  if (!require(request)) {
    install.packages("request")
    library(request)
  }

  # URL Encoding
  if (!require(urltools)) {
    install.packages("urltools")
    library(urltools)
  }
  print("Load finished.")
}
