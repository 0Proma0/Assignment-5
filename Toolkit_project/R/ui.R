library(shiny)
library(shinythemes)
library(DT)
library(plotly)
library(ggplot2)

fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("Omics Data Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("file_choice", "Select file:",
                  choices = c("TPMs_table_100genes", "Upload your own file")),
      conditionalPanel(
        condition = "input.file_choice == 'Upload your own file'",
        fileInput("file_upload", "Upload CSV/TSV file", accept = c(".csv", ".tsv"))
      ),
      uiOutput("var_select"),
      sliderInput("var_filter", "Minimum sum of expression (gene filtering):",
                  min = 0, max = 1000, value = 10),
      actionButton("run_pca", "Run PCA"),
      hr(),
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data table",
                 verbatimTextOutput("summary"),
                 DTOutput("table")
        ),
        tabPanel("QC plot (advanced)",
                 selectInput("qc_type", "QC plot type:",
                             choices = c("Density plot", "Histogram", "MA plot")),
                 conditionalPanel(
                   condition = "input.qc_type == 'MA plot'",
                   uiOutput("ma_samples")
                 ),
                 plotlyOutput("qcplot_adv")
        ),
        tabPanel("PCA", plotlyOutput("pca_plot")),
        tabPanel("Barplot",
                 selectInput("gene", "Select gene/protein:", choices = NULL),
                 plotOutput("barplot")
        ),
        tabPanel("About",
                 h4("About this application"),
                 p("This application provides several interactive plots to help analyze omics data:"),
                 tags$ol(
                   tags$li(
                     tags$b("Data Table:"),
                     " Displays the filtered numeric data in an interactive table with color coding. ",
                     tags$span(style="color:blue", "Blue colors"), " indicate lower expression values, ",
                     tags$span(style="color:red", "red colors"), " indicate higher expression values, and white indicates medium values."
                   ),
                   tags$li(
                     tags$b("QC Plot (Advanced):"),
                     " Offers quality control visualizations including:",
                     tags$ul(
                       tags$li(tags$b("Density Plot:"), " Shows the distribution of expression values for each sample, helping to identify outliers or batch effects."),
                       tags$li(tags$b("Histogram:"), " Displays the overall distribution of expression values across all samples."),
                       tags$li(tags$b("MA Plot:"), " Compares two selected samples by plotting the average expression (A) against the log2 fold change (M). Blue points represent genes/proteins with lower fold changes, while the red dashed line indicates no change (M = 0).")
                     )
                   ),
                   tags$li(
                     tags$b("PCA Plot:"),
                     " Principal Component Analysis plot that reduces data dimensionality to visualize sample clustering. Blue points represent samples, and labels help identify them."
                   ),
                   tags$li(
                     tags$b("Barplot:"),
                     " Shows expression values of a selected gene or protein across samples."
                   )
                 ),
                 hr(),
                 p(
                   tags$b("Note:"),
                   " The blue and red colors in the plots generally represent low and high expression values respectively, helping to quickly identify patterns and differences in the data."
                 )
        )
        
      )
    )
  )
)
