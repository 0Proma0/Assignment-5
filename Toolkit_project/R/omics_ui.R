library(shiny)
library(shinythemes)
library(DT)
library(shinyBS)
library(plotly)
library(ggplot2)

omics_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    theme = shinytheme("cerulean"),
    titlePanel("Omics Data Analysis"),
    sidebarLayout(
      sidebarPanel(
        selectInput(ns("file_choice"), "Select file:",
                    choices = c("TPMs_table_100genes", "Upload your own file")),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'Upload your own file'", ns("file_choice")),
          fileInput(ns("file_upload"), "Upload CSV/TSV file", accept = c(".csv", ".tsv"))
        ),
        uiOutput(ns("var_select")),
        sliderInput(ns("var_filter"), "Minimum sum of expression (gene filtering):",
                    min = 0, max = 1000, value = 0),
        actionButton(ns("run_pca"), "Run PCA"),
        hr(),
        hr()
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Data table",
                   verbatimTextOutput(ns("summary")),
                   DTOutput(ns("table"))
          ),
          tabPanel("QC plot (advanced)",
                   selectInput(ns("qc_type"), "QC plot type:",
                               choices = c("Density plot", "Histogram", "MA plot")),
                   conditionalPanel(
                     condition = sprintf("input['%s'] == 'MA plot'", ns("qc_type")),
                     uiOutput(ns("ma_samples"))
                   ),
                   plotlyOutput(ns("qcplot_adv"))
          ),
          tabPanel("PCA", plotlyOutput(ns("pca_plot"))),
          tabPanel("Barplot",
                   uiOutput(ns("gene_select")),
                   uiOutput(ns("gene_error")),
                   plotOutput(ns("barplot"), height = "400px")
          )
        )
      )
    )
  )
}