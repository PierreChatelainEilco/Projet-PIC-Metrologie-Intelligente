#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinythemes)
library(ggplot2)
library(purrr)
library(dplyr)

# # Define UI for application that draws a histogram

ui <- navbarPage('uHMM',
                   id="uHMM",
                   theme = shinytheme("cerulean"),   
                   
                  #shinytheme(theme="cerulean"),
                 
                 
                   source(file.path("ui", "importation.R"),  local = TRUE)$value,
                   source(file.path("ui", "metrologie.R"),  local = TRUE)$value,
                   source(file.path("ui", "selectionVar.R"),  local = TRUE)$value,
                   source(file.path("ui", "completion.R"),  local = TRUE)$value,
                   source(file.path("ui", "classification.R"),  local = TRUE)$value
                  )

server <- function(input, output, session) {
                  # Include the logic (server) for each tab
  globalfile <- NULL
  
  filedata <- reactive({
    inFile <- input$file
    if (is.null(inFile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    globalfile <<- read.csv(inFile$datapath, as.is=TRUE)
    return(NULL)
  })
                  source(file.path("server", "importation.R"),  local = TRUE)$value
                  source(file.path("server", "metrologie.R"),  local = TRUE)$value
                  source(file.path("server", "selectionVar.R"),  local = TRUE)$value
                  source(file.path("server", "completion.R"),  local = TRUE)$value
                  source(file.path("server", "classification.R"),  local = TRUE)$value
                }

shinyApp(ui = ui, server = server)