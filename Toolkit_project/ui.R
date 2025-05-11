library(shiny)
library(DT)

ui <- fluidPage(
  titlePanel("Advanced Data Analysis Toolkit"),
  sidebarLayout(
    sidebarPanel(
      # Panel do wczytywania danych
      wellPanel(
        h4("Data Import"),
        fileInput("file", "Choose File",
                  multiple = FALSE,
                  accept = c(".csv", ".tsv", ".txt", ".xlsx", ".xls")),
        selectInput("file_type", "File Type",
                    choices = c("Autodetect", "CSV", "TSV", "Excel", "Text (Delimited)")),
        conditionalPanel(
          condition = "input.file_type == 'CSV' || input.file_type == 'TSV' || input.file_type == 'Text (Delimited)'",
          checkboxInput("header", "Header", TRUE),
          radioButtons("sep", "Separator",
                       choices = c(Comma = ",", Semicolon = ";", Tab = "\t", Space = " "),
                       selected = ","),
          radioButtons("quote", "Quote",
                       choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"),
                       selected = '"')
        ),
        actionButton("load_data", "Load Data")
      ),
      
      # Panel do transformacji danych
      wellPanel(
        h4("Data Transformation"),
        checkboxInput("pivot_longer", "Convert to long format", FALSE),
        conditionalPanel(
          condition = "input.pivot_longer",
          selectizeInput("cols_to_pivot", "Columns to pivot", choices = NULL, multiple = TRUE),
          selectInput("names_to", "Names column", choices = NULL),
          selectInput("values_to", "Values column", choices = NULL),
          actionButton("transform_data", "Transform Data")
        )
      ),
      
      # Panel do filtrowania
      wellPanel(
        h4("Data Filtering"),
        selectInput("filter_column", "Filter by column", choices = c("None")),
        conditionalPanel(
          condition = "input.filter_column != 'None'",
          uiOutput("filter_values_ui")
        ),
        selectInput("filter_row", "Filter rows", 
                    choices = c("None", "Custom")),
        conditionalPanel(
          condition = "input.filter_row == 'Custom'",
          textInput("row_range", "Enter row range (e.g., 1:10)", value = "")
        ),
        actionButton("apply_filter", "Apply Filters")
      ),
      
      # Panel do wizualizacji
      wellPanel(
        h4("Visualization"),
        selectInput("plot_type", "Plot Type",
                    choices = c("Scatter", "Bar", "Boxplot", "Histogram", "Line", "UpSet")),
        uiOutput("plot_options_ui"),
        actionButton("generate_plot", "Generate Plot")
      )
    ),
    
    mainPanel(
      tabsetPanel(
        id = "main_tabs",
        tabPanel("Raw Data", DTOutput("raw_data")),
        tabPanel("Transformed Data", DTOutput("transformed_data")),
        tabPanel("Filtered Data", DTOutput("filtered_data")),
        tabPanel("Visualization", plotOutput("plot")),
        tabPanel("Summary", verbatimTextOutput("summary"))
      )
    )
  )
)