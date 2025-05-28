library(shiny)
library(shinyBS)
library(shinyjs)
library(shinyWidgets)
library(colourpicker)

stat_analysis_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    titlePanel("Statistical Data Analysis"),
    sidebarLayout(
      sidebarPanel(
        shinyBS::bsCollapse(
          shinyBS::bsCollapsePanel("Data Import",
                                   fileInput(ns("file"), "Choose File",
                                             accept = c(
                                               ".csv", ".tsv", ".txt", ".xlsx", ".xls",
                                               "text/csv", "text/comma-separated-values,text/plain",
                                               "application/vnd.ms-excel",
                                               "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")),
                                   selectInput(ns("file_type"), "File Type", choices = c("Autodetect", "CSV", "TSV", "Excel")),
                                   checkboxInput(ns("header"), "Header", TRUE),
                                   radioButtons(ns("sep"), "Separator", choices = c(Comma = ",", Tab = "\t", Semicolon = ";"), selected = ","),
                                   radioButtons(ns("quote"), "Quote", choices = c(None = "", "Double" = '"', "Single" = "'"), selected = '"'),
                                   actionButton(ns("load_data"), "Load Data"))),
        
        shinyBS::bsCollapse(
          shinyBS::bsCollapsePanel("Synthetic Data Generator",
        radioButtons(ns("distribution_type"), "Distribution Type",
                     choices = c("normal", "non-normal"), inline = TRUE),
        numericInput(ns("n_points"), "Number of points per group", 50, min = 10),
        numericInput(ns("n_groups"), "Number of groups", 2, min = 1),
        numericInput(ns("variation"), "Variance/Spread", 1, min = 0.1),
        checkboxInput(ns("same_settings"), "Same parameters across groups", TRUE),
        actionButton(ns("generate_data"), "Generate Synthetic Data"),
        
        hr(),
        h4("Estimators & Confidence Intervals"),
        selectInput(ns("estimator_type"), "Choose Estimator",
                    choices = c("Mean", "Trimmed Mean (10%)", "Median")),
        verbatimTextOutput(ns("bootstrap_result")),
        verbatimTextOutput(ns("monte_carlo_result")))),
        
        h4("Column Selection"),
        checkboxInput(ns("enable_column_filter"), "Enable Column Filtering", value = FALSE),
        conditionalPanel(
          condition = paste0("input['", ns("enable_column_filter"), "'] == true"),
          selectizeInput(ns("selected_columns"), "Columns to Include",
                         choices = NULL, multiple = TRUE,
                         options = list(placeholder = 'Select columns to include')),
          actionButton(ns("apply_column_filter"), "Apply Filter")
        ),
        
        hr(),
        h4("Data Transformation"),
        checkboxInput(ns("pivot"), "Pivot to long format", FALSE),
        conditionalPanel(
          condition = paste0("input['", ns("pivot"), "'] == true"),
          selectizeInput(ns("cols_to_pivot"), "Columns to Pivot", choices = NULL, multiple = TRUE),
          selectInput(ns("names_to"), "Names to", choices = NULL),
          selectInput(ns("values_to"), "Values to", choices = NULL),
          actionButton(ns("transform_data"), "Transform Data")
        ),
        
        hr(),
        h4("Test Settings"),
        numericInput(ns("alpha"), "Significance Level (α)", value = 0.05, min = 0.001, max = 0.5, step = 0.001),
        selectInput(ns("analyze_column"), "Select Column for Analysis", choices = NULL),
        actionButton(ns("analyze_distribution"), "Run Analysis"),
        actionButton(ns("reset_data"), "Reset Data"),
        hr(),
        downloadButton(ns("download_summary_txt"), "Download Summary (TXT)"),
        downloadButton(ns("download_summary_csv"), "Download Summary (CSV)"),
        
        hr(),
        shinyBS::bsCollapse(
          shinyBS::bsCollapsePanel("Plot Settings",
                                   colourpicker::colourInput(ns("fill_color"), "Fill Color", "#1f77b4"),
                                   colourpicker::colourInput(ns("border_color"), "Border Color", "#ffffff"),
                                   checkboxInput(ns("color_by_group"), "Color by Group", TRUE))),
        
        shinyBS::bsCollapse(
          shinyBS::bsCollapsePanel("Help & Feedback",
        tabPanel("Feedback",
                 fluidRow(
                   column(12,
                          textAreaInput(ns("feedback_text"), "Your message (max 200 words):", rows = 7, width = "100%"),
                          actionButton(ns("submit_feedback"), "Submit"),
                          br(), br(),
                          verbatimTextOutput(ns("feedback_status")))))))),
      
      mainPanel(
        tabsetPanel(
          tabPanel("Raw Data", DTOutput(ns("raw_data"))),
          tabPanel("Filtered Data", DTOutput(ns("filtered_data"))),
          tabPanel("Transformed Data", DTOutput(ns("transformed_data"))),
          tabPanel("Visualizations",
                   h4("Boxplot"),
                   plotOutput(ns("boxplot")),
                   h4("Histogram"),
                   plotOutput(ns("histplot"))
          ),
          tabPanel("Statistics",
                   h4("Distribution Analysis"),
                   verbatimTextOutput(ns("stats")),
                   h4("Bootstrap Analysis"),
                   plotOutput(ns("bootstrap_distribution"))
          )
        )
      )
    )
  )
}