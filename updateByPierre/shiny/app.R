## app.R ##
library(shiny)
library(shinydashboard)

source("firstTab.R")
source("importTab.R")
source("metrologyTab.R")
source("variableTab.R")
source("classificationTab_standard.R")
source("classificationTab_expert.R")
source("modelingTab.R")
source("predictionTab.R")

ui <- dashboardPage(
  dashboardHeader(title="uHMM interface"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Vue d'ensemble", tabName = "firstTab", icon = icon("dashboard")),
      menuItem("Import", tabName = "importTab", icon = icon("th")),
      menuItem("Métrologie", tabName = "metrologyTab", icon = icon("th")),
      menuItem("Selection des variables", tabName = "variableTab", icon = icon("th")),
      menuItem("Classification", tabName = "classificationTab_standard", icon = icon("th")),
      menuItem("Modélisation de séries temporelles", tabName = "modelingTab", icon = icon("th")),
      menuItem("Prédiction", tabName = "predictionTab", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(firstTab,importTab,metrologyTab,variableTab,classificationTab.standard,modelingTab,predictionTab)
    )
    
)

server <- function(input, output) { }

shinyApp(ui, server)

