library(shiny)
library(DT)
library(ggplot2)
library(plotly)
library(pheatmap)
library(reshape2)

omics_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      options(shiny.maxRequestSize = 100 * 1024^2) # 100 MB
      
      # Function to load built-in files
      load_builtin <- function(name) {
        if (name == "TPMs_table_100genes") {
          file_path <- "TPMs_table_100genes.csv"
          if (!file.exists(file_path)) {
            stop("Error: 'TPMs_table_100genes.csv' not found in the working directory.")
          }
          read.csv(file_path, row.names = 1, check.names = FALSE)
        } else {
          NULL
        }
      }
      
      # Reactive: select data
      selected_data <- reactive({
        req(input$file_choice)
        
        if (input$file_choice == "TPMs_table_100genes") {
          df <- load_builtin(input$file_choice)
          validate(need(!is.null(df) && nrow(df) > 0, "Failed to load built-in data."))
          df
        } else if (input$file_choice == "Upload your own file") {
          req(input$file_upload)
          ext <- tools::file_ext(input$file_upload$name)
          df <- if (ext == "csv") {
            read.csv(input$file_upload$datapath, row.names = 1, check.names = FALSE)
          } else if (ext == "tsv") {
            read.delim(input$file_upload$datapath, row.names = 1, check.names = FALSE)
          } else {
            NULL
          }
          validate(need(!is.null(df) && nrow(df) > 0, "Invalid or empty file uploaded."))
          df
        } else {
          NULL
        }
      })
      
      # Dynamic selection of variables for analysis
      output$var_select <- renderUI({
        df <- selected_data()
        if (is.null(df)) return(NULL)
        
        num_cols <- names(df)[sapply(df, is.numeric)]
        if (length(num_cols) == 0) {
          return(tags$span(style = "color:red", "No numeric columns in the file!"))
        }
        
        selectInput(
          ns("vars"),
          "Select variables (columns) for analysis:",
          choices = num_cols,
          selected = num_cols[1:min(length(num_cols), 2)],
          multiple = TRUE
        )
      })
      
      # Filtered data
      data_filtered <- reactive({
        df <- selected_data()
        req(df)
        
        num_cols <- names(df)[sapply(df, is.numeric)]
        if (length(num_cols) == 0) return(NULL)
        
        if (is.null(input$vars) || !any(input$vars %in% num_cols)) return(NULL)
        
        df <- df[, input$vars, drop = FALSE]
        df <- df[, sapply(df, is.numeric), drop = FALSE]
        if (ncol(df) == 0) return(NULL)
        
        df[rowSums(df) >= input$var_filter, , drop = FALSE]
      })
      
      # Export data
      output$downloadData <- downloadHandler(
        filename = function() {
          paste0(input$file_choice, ".csv")
        },
        content = function(file) {
          df <- data_filtered()
          if (is.null(df) || ncol(df) == 0) {
            write.csv(data.frame(Message = "No numeric columns to export!"), file, row.names = FALSE)
          } else {
            write.csv(df, file)
          }
        }
      )
      
      # Statistics
      output$summary <- renderPrint({
        df <- data_filtered()
        if (is.null(df) || nrow(df) == 0 || ncol(df) == 0) {
          return("No numeric data for analysis.")
        }
        summary(df)
      })
      
      # Colored table
      output$table <- renderDT({
        df <- data_filtered()
        if (is.null(df) || ncol(df) == 0) {
          return(datatable(data.frame(Message = "No numeric columns for analysis!")))
        }
        
        datatable(df, options = list(pageLength = 10)) %>%
          formatStyle(
            columns = names(df),
            backgroundColor = styleInterval(
              quantile(unlist(df), probs = c(0.25, 0.75), na.rm = TRUE),
              c("lightblue", "white", "salmon")
            )
          )
      })
      
      # UI for MA plot sample selection
      output$ma_samples <- renderUI({
        df <- data_filtered()
        if (is.null(df) || ncol(df) < 2) {
          return(tags$span(style = "color:red", "Not enough numeric columns for MA plot!"))
        }
        
        tagList(
          selectInput(
            ns("ma_sample1"),
            "Sample 1:",
            choices = colnames(df),
            selected = colnames(df)[1]
          ),
          selectInput(
            ns("ma_sample2"),
            "Sample 2:",
            choices = colnames(df),
            selected = colnames(df)[2]
          )
        )
      })
      
      # Advanced QC plots with ggplot2 and plotly
      output$qcplot_adv <- renderPlotly({
        req(input$qc_type)
        df <- data_filtered()
        if (is.null(df) || nrow(df) == 0 || ncol(df) == 0) return(NULL)
        
        if (input$qc_type == "Density plot") {
          mat <- as.matrix(df)
          df_melt <- melt(mat)
          colnames(df_melt) <- c("Gene", "Sample", "Expression")
          p <- ggplot(df_melt, aes(x = Expression, color = Sample)) +
            geom_density() +
            theme_minimal() +
            labs(title = "Density plot (each sample)", x = "Expression", y = "Density")
          ggplotly(p)
        } else if (input$qc_type == "Histogram") {
          p <- ggplot(data.frame(Expression = unlist(df)), aes(x = Expression)) +
            geom_histogram(bins = 40, fill = "gray", color = "black") +
            theme_minimal() +
            labs(title = "Expression histogram (all samples)", x = "Expression", y = "Count")
          ggplotly(p)
        } else if (input$qc_type == "MA plot") {
          req(input$ma_sample1, input$ma_sample2)
          x <- as.numeric(df[, input$ma_sample1])
          y <- as.numeric(df[, input$ma_sample2])
          M <- log2(y + 1) - log2(x + 1)
          A <- 0.5 * (log2(x + 1) + log2(y + 1))
          df_ma <- data.frame(A = A, M = M)
          p <- ggplot(df_ma, aes(x = A, y = M)) +
            geom_point(color = "steelblue") +
            geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
            theme_minimal() +
            labs(
              title = paste("MA plot:", input$ma_sample1, "vs", input$ma_sample2),
              x = "A = mean log2 expression",
              y = "M = log2 fold change"
            )
          ggplotly(p)
        }
      })
      
      # PCA
      pca_res <- eventReactive(input$run_pca, {
        df <- data_filtered()
        if (is.null(df) || nrow(df) < 3 || ncol(df) < 2) return(NULL)
        prcomp(t(df), scale. = TRUE)
      })
      
      output$pca_plot <- renderPlotly({
        pca <- pca_res()
        if (is.null(pca)) return(NULL)
        
        pc_df <- as.data.frame(pca$x)
        pc_df$Sample <- rownames(pc_df)
        p <- ggplot(pc_df, aes(x = PC1, y = PC2, label = Sample)) +
          geom_point(size = 3, color = "#2c3e50") +
          geom_text(nudge_y = 1.5, size = 3) +
          labs(title = "PCA", x = "PC1", y = "PC2") +
          theme_minimal()
        ggplotly(p)
      })
      
      # Barplot for selected gene/protein
      observe({
        df <- data_filtered()
        if (is.null(df) || nrow(df) == 0) {
          updateSelectInput(session, ns("gene"), choices = character(0))
        } else {
          updateSelectInput(session, ns("gene"), choices = rownames(df))
        }
      })
      
      output$barplot <- renderPlot({
        df <- data_filtered()
        if (is.null(df) || is.null(input$gene) || !(input$gene %in% rownames(df))) return()
        
        barplot(
          as.numeric(df[input$gene, ]),
          names.arg = colnames(df),
          main = paste("Expression", input$gene),
          col = "steelblue",
          ylab = "Expression"
        )
      })
      
      # Debug output
      output$debug <- renderPrint({
        cat("input$file_choice:", input$file_choice, "\n")
        cat("selected_data() is NULL:", is.null(selected_data()), "\n")
        cat("data_filtered() is NULL:", is.null(data_filtered()), "\n")
      })
    }
  )
}