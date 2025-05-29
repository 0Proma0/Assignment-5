library(shiny)
library(shinydashboard)
library(rsconnect)
# Load modules
source("R/stat_analysis_ui.R")
source("R/stat_analysis_server.R")
source("R/omics_ui.R")
source("R/omics_server.R")


ui <- dashboardPage(
  dashboardHeader(title = "Data Analysis Toolkit"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Statistical Analysis", tabName = "stat_analysis", icon = icon("chart-bar")),
      menuItem("Omics Analysis", tabName = "omics_analysis", icon = icon("dna")),
      menuItem("BLAST & Hits", tabName = "blast_analysis", icon = icon("search"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "stat_analysis",
              stat_analysis_ui("stat")),
      tabItem(tabName = "omics_analysis",
              omics_ui("omics")),
      tabItem(tabName = "blast_analysis",
              h3("To be implemented..."))
    )
  )
)

server <- function(input, output, session) {
  stat_analysis_server("stat")
  omics_server("omics")
}

shinyApp(ui, server)
