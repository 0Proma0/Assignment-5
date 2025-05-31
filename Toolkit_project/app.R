library(shiny)
library(shinydashboard)
library(rsconnect)
library(httr)
library(jsonlite)
library(DT)
library(ggplot2)
library(colourpicker)

# Load modules
source("R/stat_analysis_ui.R")
source("R/stat_analysis_server.R")
source("R/omics_ui.R")
source("R/omics_server.R")
source("R/blast_ui.R")
source("R/blast_server.R")


ui <- dashboardPage(
  dashboardHeader(title = "Data Analysis Toolkit"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Statistical Analysis", tabName = "stat_analysis", icon = icon("chart-bar")),
      menuItem("Omics Analysis", tabName = "omics_analysis", icon = icon("dna")),
      menuItem("BLAST & Hits", tabName = "blast_analysis", icon = icon("search")),
      menuItem("GitHub here!", icon = icon("github"), href = "https://github.com/0Proma0/Assignments-for-Toolkit")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "stat_analysis",
              stat_analysis_ui("stat")
              ),
      tabItem(tabName = "omics_analysis",
              omics_ui("omics")
              ),
      tabItem(tabName = "blast_analysis",
              blast_ui("blast")
              )
    )
  )
)

server <- function(input, output, session) {
  stat_analysis_server("stat")
  omics_server("omics")
  blast_server("blast")
}

shinyApp(ui, server)
