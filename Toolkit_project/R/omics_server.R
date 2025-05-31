library(shiny)
library(DT)
library(ggplot2)
library(plotly)
library(pheatmap)
library(reshape2)

omics_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    options(shiny.maxRequestSize = 100 * 1024^2) # 100 MB
    
    # Function to load built-in files
    load_builtin <- function(name) {
      if (name == "TPMs_table_100genes") {
        file_path <- file.path("data", "TPMs_table_100genes.csv")
        message("Attempting to load file: ", file_path, " (exists: ", file.exists(file_path), ")")
        tryCatch(
          {
            if (!file.exists(file_path)) {
              message("File not found: ", file_path)
              return(NULL)
            }
            df <- read.csv(file_path, row.names = 1, check.names = FALSE)
            message("Loaded columns: ", paste(colnames(df), collapse = ", "))
            message("Column types: ", paste(sapply(df, class), collapse = ", "))
            df
          },
          error = function(e) {
            message("Error loading built-in file: ", e$message)
            return(NULL)
          }
        )
      } else {
        NULL
      }
    }
    
    # Reactive: select data
    selected_data <- reactive({
      message("input$file_choice: ", input$file_choice)
      req(input$file_choice)
      if (input$file_choice == "TPMs_table_100genes") {
        df <- load_builtin(input$file_choice)
        message("Built-in df is NULL: ", is.null(df))
        message("Built-in df rows: ", if (!is.null(df)) nrow(df) else "NULL")
        shiny::validate(
          need(!is.null(df) && nrow(df) > 0, "Failed to load 'TPMs_table_100genes.csv' or the file is empty. Please ensure the file exists in the 'data' directory.")
        )
        df
      } else if (input$file_choice == "Upload your own file") {
        req(input$file_upload)
        ext <- tools::file_ext(input$file_upload$name)
        message("Uploaded file extension: ", ext)
        df <- if (ext == "csv") {
          read.csv(input$file_upload$datapath, row.names = 1, check.names = FALSE)
        } else if (ext == "tsv") {
          read.delim(input$file_upload$datapath, row.names = 1, check.names = FALSE)
        } else {
          NULL
        }
        message("Uploaded df is NULL: ", is.null(df))
        message("Uploaded df rows: ", if (!is.null(df)) nrow(df) else "NULL")
        message("Uploaded columns: ", if (!is.null(df)) paste(colnames(df), collapse = ", ") else "NULL")
        message("Uploaded column types: ", if (!is.null(df)) paste(sapply(df, class), collapse = ", ") else "NULL")
        shiny::validate(
          need(!is.null(df) && nrow(df) > 0, "Invalid or empty file uploaded. Please upload a valid CSV or TSV file.")
        )
        df
      } else {
        NULL
      }
    })
    
    # Dynamic selection of variables for analysis
    output$var_select <- renderUI({
      df <- selected_data()
      message("var_select: df is NULL: ", is.null(df))
      if (is.null(df)) return(NULL)
      num_cols <- names(df)[sapply(df, is.numeric)]
      message("var_select: Numeric columns: ", length(num_cols), " (", paste(num_cols, collapse = ", "), ")")
      if (length(num_cols) == 0) {
        return(tags$span(style = "color:red", "No numeric columns in the file!"))
      }
      selectInput(ns("vars"), "Select variables (columns) for analysis:",
                  choices = num_cols, selected = num_cols, multiple = TRUE)
    })
    
    # Filtered data
    data_filtered <- reactive({
      df <- selected_data()
      req(df)
      message("data_filtered: selected_data rows: ", nrow(df))
      num_cols <- names(df)[sapply(df, is.numeric)]
      message("data_filtered: Numeric columns: ", length(num_cols))
      if (length(num_cols) == 0) {
        message("data_filtered: No numeric columns found")
        return(NULL)
      }
      if (is.null(input$vars) || !any(input$vars %in% num_cols)) {
        message("data_filtered: input$vars: ", if (is.null(input$vars)) "NULL" else paste(input$vars, collapse = ", "))
        message("data_filtered: No valid vars selected")
        return(NULL)
      }
      df <- df[, input$vars, drop = FALSE]
      df <- df[, sapply(df, is.numeric), drop = FALSE]
      message("data_filtered: Filtered df columns: ", ncol(df))
      if (ncol(df) == 0) {
        message("data_filtered: No numeric columns after filtering")
        return(NULL)
      }
      df_filtered <- df[rowSums(df) >= input$var_filter, , drop = FALSE]
      message("data_filtered: Filtered df rows: ", nrow(df_filtered))
      message("data_filtered: var_filter value: ", input$var_filter)
      # Ensure row names are preserved
      if (nrow(df_filtered) > 0 && (is.null(rownames(df_filtered)) || length(rownames(df_filtered)) == 0)) {
        rownames(df_filtered) <- rownames(df)[rowSums(df) >= input$var_filter]
        message("data_filtered: Assigned row names: ", paste(rownames(df_filtered), collapse = ", "))
      }
      df_filtered
    })
    
    # Export data
    output$downloadData <- downloadHandler(
      filename = function() { paste0(input$file_choice, ".csv") },
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
      message("summary: df is NULL: ", is.null(df))
      if (is.null(df) || nrow(df) == 0 || ncol(df) == 0) return("No numeric data for analysis.")
      summary(df)
    })
    
    # Colored table
    output$table <- renderDT({
      df <- data_filtered()
      message("table: df is NULL: ", is.null(df))
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
      message("ma_samples: df is NULL: ", is.null(df))
      if (is.null(df) || ncol(df) < 2) return(tags$span(style = "color:red", "Not enough numeric columns for MA plot!"))
      tagList(
        selectInput(ns("ma_sample1"), "Sample 1:", choices = colnames(df), selected = colnames(df)[1]),
        selectInput(ns("ma_sample2"), "Sample 2:", choices = colnames(df), selected = colnames(df)[2])
      )
    })
    
    # Advanced QC plots with ggplot2 and plotly
    output$qcplot_adv <- renderPlotly({
      req(input$qc_type)
      df <- data_filtered()
      message("qcplot_adv: df is NULL: ", is.null(df))
      message("qcplot_adv: qc_type: ", input$qc_type)
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
          labs(title = paste("MA plot:", input$ma_sample1, "vs", input$ma_sample2),
               x = "A = mean log2 expression", y = "M = log2 fold change")
        ggplotly(p)
      }
    })
    
    # PCA
    pca_res <- eventReactive(input$run_pca, {
      df <- data_filtered()
      message("pca_res: df is NULL: ", is.null(df))
      if (is.null(df) || nrow(df) < 3 || ncol(df) < 2) return(NULL)
      prcomp(t(df), scale. = TRUE)
    })
    
    output$pca_plot <- renderPlotly({
      pca <- pca_res()
      message("pca_plot: pca is NULL: ", is.null(pca))
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
    output$gene_select <- renderUI({
      df <- data_filtered()
      message("gene_select: df is NULL: ", is.null(df))
      message("gene_select: df rows: ", if (is.null(df)) "NULL" else nrow(df))
      message("gene_select: df rownames: ", if (is.null(df)) "NULL" else paste(rownames(df), collapse = ", "))
      if (is.null(df) || nrow(df) == 0 || is.null(rownames(df)) || length(rownames(df)) == 0) {
        return(NULL)
      }
      selectInput(ns("gene"), "Select gene/protein:", choices = rownames(df), selected = rownames(df)[1])
    })
    
    output$gene_error <- renderUI({
      df <- data_filtered()
      message("gene_error: df is NULL: ", is.null(df))
      if (is.null(df) || nrow(df) == 0 || is.null(rownames(df)) || length(rownames(df)) == 0) {
        tags$span(style = "color:red", "No genes available. Ensure data has valid row names and numeric columns.")
      } else {
        NULL
      }
    })
    
    output$barplot <- renderPlot({
      df <- data_filtered()
      message("barplot: df is NULL: ", is.null(df))
      message("barplot: df rows: ", if (is.null(df)) "NULL" else nrow(df))
      message("barplot: df rownames: ", if (is.null(df)) "NULL" else paste(rownames(df), collapse = ", "))
      message("barplot: input$gene: ", if (is.null(input$gene)) "NULL" else input$gene)
      message("barplot: input$gene valid: ", if (is.null(df) || is.null(input$gene)) "FALSE" else input$gene %in% rownames(df))
      if (is.null(df) || nrow(df) == 0 || is.null(input$gene) || !(input$gene %in% rownames(df))) {
        message("barplot: Skipping render due to invalid df or gene")
        return()
      }
      barplot(as.numeric(df[input$gene, ]), 
              names.arg = colnames(df), 
              main = paste("Expression", input$gene),
              col = "steelblue", ylab = "Expression")
    })
  })
}