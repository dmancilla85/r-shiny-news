#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("./src/libraries.R", local = TRUE, encoding = c("UTF-8"))
source("ui.R", local = TRUE, encoding = c("UTF-8"))
source("server.R", local = TRUE, encoding = c("UTF-8"))

# Run the application
shinyApp(ui = ui, server = server)
