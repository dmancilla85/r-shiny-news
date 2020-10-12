
# Sentiment Analysis
if (!require(syuzhet)) {
  install.packages("syuzhet")
  library(syuzhet)
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