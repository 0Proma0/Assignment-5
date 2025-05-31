library(shiny)
library(httr)
library(jsonlite)
library(DT)
library(ggplot2)
library(colourpicker)

blast_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    titlePanel("Protein BLAST Toolkit"),
    tabsetPanel(
      tabPanel("Download Data",
               sidebarLayout(
                 sidebarPanel(
                   textInput(ns("protein_name"), "Protein name:", value = "p53"),
                   textInput(ns("organism"), "Organism name:", value = "Homo sapiens"),
                   actionButton(ns("search"), "Search proteins"),
                   uiOutput(ns("protein_select_ui")),
                   fileInput(ns("fasta_file"), "Or upload FASTA file:", accept = ".fasta"),
                   actionButton(ns("get_fasta"), "Load FASTA"),
                   downloadButton(ns("download_fasta"), "Download FASTA"),
                   br(), br(),
                   actionButton(ns("run_blast"), "Run BLAST")
                 ),
                 mainPanel(
                   verbatimTextOutput(ns("fasta_output")),
                   br(),
                   uiOutput(ns("blast_status"))
                 )
               )
      ),
      tabPanel("Results Table",
               sidebarLayout(
                 sidebarPanel(
                   checkboxGroupInput(ns("columns_to_show"), "Select columns to display:",
                                    choices = c("Hit_ID", "Description", "Length", "Identity", "E_value",
                                               "Bit_score", "Align_len", "Query_from", "Query_to",
                                               "Hit_from", "Hit_to", "Accession", "Organism", "Gene", "URL"),
                                    selected = c("Hit_ID", "Description", "E_value")),
                   sliderInput(ns("length_range"), "Length range:", min = 0, max = 1000, value = c(0, 1000)),
                   sliderInput(ns("identity_range"), "Identity [%]:", min = 0, max = 100, value = c(0, 100)),
                   sliderInput(ns("evalue_range"), "E-value range (log10):", min = -100, max = 0, value = c(-100, 0)),
                   downloadButton(ns("download_results"), "Download Results (CSV)")
                 ),
                 mainPanel(
                   DTOutput(ns("blast_table"))
                 )
               )
      ),
      tabPanel("Plots",
               sidebarLayout(
                 sidebarPanel(
                   selectInput(ns("plot_type"), "Plot Type",
                             choices = c("Histogram" = "hist", "Scatterplot" = "scatter", "Boxplot" = "boxplot")),
                   uiOutput(ns("plot_settings")),
                   sliderInput(ns("plot_bins"), "Number of bins (for histogram):", min = 5, max = 100, value = 30),
                   colourpicker::colourInput(ns("plot_fill"), "Fill color", value = "#1f77b4"),
                   colourpicker::colourInput(ns("plot_border"), "Border color", value = "#0F0F0F"),
                   sliderInput(ns("plot_border_size"), "Border/line thickness:", min = 0.5, max = 4, value = 1, step = 0.1),
                   sliderInput(ns("point_size"), "Point/box size:", min = 1, max = 8, value = 3),
                   downloadButton(ns("download_plot"), "Download Plot (PNG)")
                 ),
                 mainPanel(
                   plotOutput(ns("blast_plot"))
                 )
               )
      )
    )
  )
}