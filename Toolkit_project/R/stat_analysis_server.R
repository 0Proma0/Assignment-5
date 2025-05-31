library(shiny)
library(tidyverse)
library(readxl)
library(DT)
library(car)
library(shiny.i18n)
library(colourpicker)
library(viridis)
library(httr)

stat_analysis_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    rv <- reactiveValues(
      raw_data = NULL,
      column_filtered_data = NULL,
      transformed_data = NULL,
      filtered_data = NULL,
      available_columns = NULL,
      is_long_format = FALSE
    )
    
    summary_table <- reactiveVal(NULL)
    
    # Generate synthetic data
    observeEvent(input$generate_data, {
      n <- input$n_points
      groups <- input$n_groups
      var <- input$variation
      same_settings <- input$same_settings
      synthetic <- data.frame()
      
      for (g in 1:groups) {
        mean_val <- if (same_settings) 0 else rnorm(1, 5, 2)
        sd_val <- if (same_settings) var else abs(rnorm(1, var, 1))
        x <- if (input$distribution_type == "normal") {
          rnorm(n, mean = mean_val, sd = sd_val)
        } else {
          rexp(n, rate = 1 / sd_val)
        }
        synthetic <- rbind(synthetic, data.frame(Value = x, Group = as.factor(paste0("G", g))))
      }
      
      rv$raw_data <- synthetic
      rv$transformed_data <- NULL
      rv$filtered_data <- NULL
      rv$is_long_format <- FALSE
      updateSelectInput(session, "analyze_column", choices = names(synthetic))
      updateSelectizeInput(session, "selected_columns", choices = names(synthetic), selected = names(synthetic))
    })
    
    # Load data
    observeEvent(input$load_data, {
      req(input$file)
      tryCatch({
        ext <- tools::file_ext(input$file$name)
        file_type <- if (input$file_type == "Autodetect") ext else tolower(input$file_type)
        
        if (file_type %in% c("csv", "tsv", "txt")) {
          sep <- if (file_type == "tsv") "\t" else input$sep
          rv$raw_data <- readr::read_delim(input$file$datapath,
                                           delim = sep,
                                           quote = input$quote,
                                           col_names = input$header)
        } else if (file_type %in% c("xlsx", "xls")) {
          rv$raw_data <- read_excel(input$file$datapath)
        }
        
        # Check if data is already in long format
        numeric_cols <- sapply(rv$raw_data, is.numeric)
        rv$is_long_format <- sum(numeric_cols) == 1 && 
          any(sapply(rv$raw_data, function(x) is.factor(x) || is.character(x)))
        
        updateSelectizeInput(session, "cols_to_pivot", 
                             choices = names(rv$raw_data), 
                             selected = if(!rv$is_long_format) names(rv$raw_data)[numeric_cols])
        updateSelectInput(session, "names_to", choices = c("variable", names(rv$raw_data)))
        updateSelectInput(session, "values_to", choices = c("value", names(rv$raw_data)))
        updateSelectInput(session, "analyze_column", 
                          choices = names(rv$raw_data),
                          selected = if(rv$is_long_format && "value" %in% names(rv$raw_data)) "value" else names(rv$raw_data)[1])
        updateSelectizeInput(session, "selected_columns", 
                             choices = names(rv$raw_data), 
                             selected = names(rv$raw_data))
        
      }, error = function(e) {
        showNotification(paste("Error loading file:", e$message), type = "error")
      })
    })
    
    # Transform to long format
    observeEvent(input$transform_data, {
      req(input$pivot, (rv$column_filtered_data %||% rv$raw_data), input$cols_to_pivot)
      tryCatch({
        data_to_transform <- if (!is.null(rv$column_filtered_data)) {
          rv$column_filtered_data
        } else {
          rv$raw_data
        }
        
        rv$transformed_data <- data_to_transform %>%
          pivot_longer(cols = input$cols_to_pivot,
                       names_to = input$names_to,
                       values_to = input$values_to)
        rv$is_long_format <- TRUE
      }, error = function(e) {
        showNotification(paste("Transformation error:", e$message), type = "error")
      })
    })
    
    # Apply column filtering
    observeEvent(input$apply_column_filter, {
      req(input$enable_column_filter, rv$raw_data)
      
      if (!is.null(input$selected_columns) && length(input$selected_columns) > 0) {
        rv$column_filtered_data <- rv$raw_data %>% 
          select(all_of(input$selected_columns))
        rv$is_long_format <- FALSE
      } else {
        rv$column_filtered_data <- NULL
      }
    })
    
    # Reactive dataset
    active_data <- reactive({
      if (!is.null(rv$transformed_data)) {
        rv$transformed_data
      } else if (!is.null(rv$column_filtered_data)) {
        rv$column_filtered_data
      } else {
        rv$raw_data
      }
    })
    
    find_group_var <- function(data, exclude_col = NULL) {
      if (is.null(data)) return(NULL)
      
      # First look for obvious group columns
      group_candidates <- names(data)[sapply(data, function(x) {
        (is.factor(x) || is.character(x)) && 
          n_distinct(x) >= 2 && 
          !identical(x, exclude_col)
      })]  # Fixed: Added missing closing bracket
      
      # If none found, look for columns with low cardinality
      if (length(group_candidates) == 0) {
        group_candidates <- names(data)[sapply(data, function(x) {
          n_distinct(x) <= 10 && 
            n_distinct(x) >= 2 && 
            !identical(x, exclude_col)
        })]
      }
      
      # Return first candidate or NULL
      if (length(group_candidates) > 0) group_candidates[1] else NULL
    }
    
    # Update column selections based on current data state
    observe({
      current_data <- if (!is.null(rv$column_filtered_data)) {
        rv$column_filtered_data
      } else {
        rv$raw_data
      }
      
      if (!is.null(current_data)) {
        updateSelectizeInput(session, "cols_to_pivot", 
                             choices = names(current_data))
      }
    })
    
    # Update analysis column choices
    observe({
      req(active_data())
      data <- active_data()
      cols <- names(data)
      
      # Prefer 'value' column if exists (long format)
      selected <- if ("value" %in% cols) "value" else cols[1]
      
      updateSelectInput(session, "analyze_column", 
                        choices = cols,
                        selected = selected)
    })
    
    # Bootstrap analysis
    output$bootstrap_result <- renderPrint({
      req(active_data(), input$analyze_column)
      stat <- switch(input$estimator_type,
                     "Mean" = function(x) mean(x, na.rm = TRUE),
                     "Trimmed Mean (10%)" = function(x) mean(x, trim = 0.1, na.rm = TRUE),
                     "Median" = function(x) median(x, na.rm = TRUE))
      data <- active_data()[[input$analyze_column]]
      if (all(is.na(data))) {
        cat("No valid data to analyze\n")
        return()
      }
      
      B <- 1000
      boot_vals <- replicate(B, stat(sample(data, replace = TRUE)))
      ci <- quantile(boot_vals, c(0.025, 0.975), na.rm = TRUE)
      cat("Bootstrap Estimate of Mean (μ^) = sum(x)/n\n")
      cat("Mean Estimate:", round(mean(boot_vals, na.rm = TRUE), 4), "\n")
      cat("95% Confidence Interval:", round(ci[1], 4), "-", round(ci[2], 4), "\n")
    })
    
    output$bootstrap_distribution <- renderPlot({
      req(active_data(), input$analyze_column)
      data <- active_data()[[input$analyze_column]]
      if (all(is.na(data))) return(NULL)
      
      stat <- switch(input$estimator_type,
                     "Mean" = function(x) mean(x, na.rm = TRUE),
                     "Trimmed Mean (10%)" = function(x) mean(x, trim = 0.1, na.rm = TRUE),
                     "Median" = function(x) median(x, na.rm = TRUE))
      boot_vals <- replicate(1000, stat(sample(data, replace = TRUE)))
      
      ggplot(data.frame(x = boot_vals), aes(x)) +
        geom_histogram(bins = 30, fill = "skyblue", color = "white") +
        geom_vline(xintercept = mean(boot_vals, na.rm = TRUE), linetype = "dashed") +
        labs(title = "Bootstrap Distribution of Estimator")
    })
    
    # Monte Carlo analysis
    output$monte_carlo_result <- renderPrint({
      req(active_data(), input$analyze_column)
      data <- active_data()[[input$analyze_column]]
      if (all(is.na(data))) {
        cat("No valid data to analyze\n")
        return()
      }
      
      B <- 1000
      n <- length(data)
      sim_vals <- replicate(B, {
        sim <- rnorm(n, mean(data, na.rm = TRUE), sd(data, na.rm = TRUE))
        mean(sim)
      })
      ci <- quantile(sim_vals, c(0.025, 0.975), na.rm = TRUE)
      cat("Monte Carlo Estimate of Mean:", round(mean(sim_vals, na.rm = TRUE), 4), "\n")
      cat("95% Confidence Interval:", round(ci[1], 4), "-", round(ci[2], 4), "\n")
    })
    
    # Data tables
    output$raw_data <- renderDT({
      req(rv$raw_data)
      datatable(rv$raw_data, options = list(scrollX = TRUE))
    })
    
    output$filtered_data <- renderDT({
      req(rv$column_filtered_data)
      datatable(rv$column_filtered_data, options = list(scrollX = TRUE))
    })
    
    output$transformed_data <- renderDT({
      req(rv$transformed_data)
      datatable(rv$transformed_data, options = list(scrollX = TRUE))
    })
    
    # Visualizations
    output$boxplot <- renderPlot({
      req(active_data(), input$analyze_column)
      data <- active_data()
      y_col <- input$analyze_column
      
      # Find grouping column
      group_var <- if(input$color_by_group) find_group_var(data, y_col) else NULL
      
      p <- ggplot(data, aes_string(y = y_col)) +
        theme_minimal()
      
      if(!is.null(group_var)) {
        p <- p + aes_string(x = group_var, fill = group_var) +
          geom_boxplot(color = input$border_color) +
          scale_fill_viridis_d() +
          theme(legend.position = "bottom")
      } else {
        p <- p + geom_boxplot(fill = input$fill_color, color = input$border_color)
      }
      
      p
    })
    
    output$histplot <- renderPlot({
      req(active_data(), input$analyze_column)
      data <- active_data()
      y <- data[[input$analyze_column]]
      
      if (!is.numeric(y) || length(unique(y)) < 2) return(NULL)
      
      # Detect grouping column
      group_var <- NULL
      for (col in names(data)) {
        if (col == input$analyze_column) next
        if ((is.factor(data[[col]]) || is.character(data[[col]])) && n_distinct(data[[col]]) >= 2) {
          group_var <- col
          break
        }
      }
      
      p <- ggplot(data, aes_string(x = input$analyze_column))
      
      # Apply group coloring if enabled and group_var exists
      if (!is.null(group_var) && input$color_by_group) {
        p <- p + aes_string(fill = group_var)
      } else {
        p <- p + aes(fill = I(input$fill_color))
      }
      
      p <- p + geom_histogram(bins = 30, color = input$border_color, alpha = 0.8)
      
      # Add facets if grouped
      if (!is.null(group_var)) {
        p <- p + facet_wrap(as.formula(paste("~", group_var)))
      }
      
      p + theme_minimal() +
        guides(fill = guide_legend(title = group_var)) +
        labs(title = "Histogram", x = input$analyze_column, y = "Count")
    })
    
    
    # Statistical analysis
    observeEvent(input$analyze_distribution, {
      req(active_data(), input$analyze_column)
      
      data <- active_data()
      y <- data[[input$analyze_column]]
      
      # Remove NA values
      y <- y[!is.na(y)]
      if (length(y) < 3) {
        showNotification("Not enough data points for analysis (need at least 3)", type = "warning")
        return()
      }
      
      # Find grouping variable
      group_var <- find_group_var(data, input$analyze_column)
      
      # Normality test
      normal_p <- NA
      normality_message <- "Normality test not performed (sample size requirements not met)"
      if (length(y) >= 3 && length(y) <= 5000) {
        normal_test <- shapiro.test(y)
        normal_p <- normal_test$p.value
        normality_message <- ifelse(normal_p > input$alpha,
                                    paste("✅ Data appears normally distributed (Shapiro-Wilk p =", round(normal_p, 4), ")"),
                                    paste("❌ Data does NOT appear normally distributed (Shapiro-Wilk p =", round(normal_p, 4), ")"))
      }
      
      # Homogeneity of variance test
      levene_p <- NA
      variance_message <- "Variance test not performed (no suitable grouping variable found)"
      if (!is.null(group_var) && n_distinct(data[[group_var]]) >= 2) {
        formula <- as.formula(paste(input$analyze_column, "~", group_var))
        levene_result <- try(car::leveneTest(formula, data = data), silent = TRUE)
        if (!inherits(levene_result, "try-error")) {
          levene_p <- levene_result$`Pr(>F)`[1]
          variance_message <- ifelse(levene_p > input$alpha,
                                     paste("✅ Variances appear equal across groups (Levene's p =", round(levene_p, 4), ")"),
                                     paste("❌ Variances differ significantly across groups (Levene's p =", round(levene_p, 4), ")"))
        }
      }
      
      # Determine and perform appropriate test
      test_name <- "No test performed"
      test_result <- NULL
      pval <- NA
      test_explanation <- "No suitable test could be determined"
      ngroups <- if (!is.null(group_var)) length(unique(na.omit(data[[group_var]]))) else 0
      
      if (ngroups == 2) {
        formula <- as.formula(paste(input$analyze_column, "~", group_var))
        
        if (!is.na(normal_p) && normal_p > input$alpha) {
          if (!is.na(levene_p) && levene_p > input$alpha) {
            test_name <- "Student's t-test"
            test_result <- t.test(formula, data = data, var.equal = TRUE)
            test_explanation <- "Used Student's t-test because:
            - Data is normally distributed
            - Variances are equal between groups
            - Comparing exactly 2 groups"
          } else {
            test_name <- "Welch's t-test"
            test_result <- t.test(formula, data = data)
            test_explanation <- "Used Welch's t-test because:
            - Data is normally distributed
            - Variances are UNEQUAL between groups
            - Comparing exactly 2 groups"
          }
        } else {
          test_name <- "Wilcoxon rank-sum test"
          test_result <- wilcox.test(formula, data = data)
          test_explanation <- "Used Wilcoxon rank-sum test because:
          - Data is NOT normally distributed
          - Non-parametric test required
          - Comparing exactly 2 groups"
        }
        pval <- test_result$p.value
        
      } else if (ngroups > 2) {
        formula <- as.formula(paste(input$analyze_column, "~", group_var))
        
        if (!is.na(normal_p) && normal_p > input$alpha) {
          if (!is.na(levene_p) && levene_p > input$alpha) {
            test_name <- "One-way ANOVA"
            test_result <- aov(formula, data = data)
            pval <- summary(test_result)[[1]]$'Pr(>F)'[1]
            test_explanation <- "Used ANOVA because:
            - Data is normally distributed
            - Variances are equal across groups
            - Comparing more than 2 groups"
          } else {
            test_name <- "Welch's ANOVA"
            test_result <- oneway.test(formula, data = data, var.equal = FALSE)
            pval <- test_result$p.value
            test_explanation <- "Used Welch's ANOVA because:
            - Data is normally distributed
            - Variances are UNEQUAL across groups
            - Comparing more than 2 groups"
          }
        } else {
          test_name <- "Kruskal-Wallis test"
          test_result <- kruskal.test(formula, data = data)
          pval <- test_result$p.value
          test_explanation <- "Used Kruskal-Wallis test because:
          - Data is NOT normally distributed
          - Non-parametric test required
          - Comparing more than 2 groups"
        }
      }
      
      # Interpretation of p-value
      interpretation <- if (!is.na(pval)) {
        if (pval < input$alpha) {
          paste("❗ Significant difference found (p =", round(pval, 4), 
                "which is < α =", input$alpha, ")")
        } else {
          paste("✔️ No significant difference found (p =", round(pval, 4), 
                "which is ≥ α =", input$alpha, ")")
        }
      } else {
        "No p-value available for interpretation"
      }
      
      # Create detailed display table
      display_table <- data.frame(
        Section = c(
          "Data Characteristics", "Data Characteristics", 
          "Test Selection", "Test Selection", "Test Selection",
          "Results", "Results", "Results"
        ),
        Metric = c(
          "Normality", "Variance Equality", 
          "Test Chosen", "Test Reason", "Test Assumptions",
          "p-value", "Significance Level (α)", "Conclusion"
        ),
        Value = c(
          normality_message,
          variance_message,
          test_name,
          test_explanation,
          if (!is.null(test_result)) paste(capture.output(print(test_result$method)), collapse = " ") else "N/A",
          if (!is.na(pval)) round(pval, 4) else "N/A",
          input$alpha,
          interpretation
        ),
        stringsAsFactors = FALSE
      )
      
      # Create minimal download table
      download_table <- data.frame(
        Statistic = c(
          "normality_p_value",
          "variance_p_value",
          "test_name",
          "test_p_value",
          "significance_level",
          "significant"
        ),
        Value = c(
          ifelse(!is.na(normal_p), round(normal_p, 6), NA),
          ifelse(!is.na(levene_p), round(levene_p, 6), NA),
          test_name,
          ifelse(!is.na(pval), round(pval, 6), NA),
          input$alpha,
          ifelse(!is.na(pval), pval < input$alpha, NA)
        ),
        stringsAsFactors = FALSE
      )
      
      # Store both tables
      summary_table(display_table)
      rv$download_table <- download_table
    })
    
    # Render stats output with detailed formatting
    output$stats <- renderPrint({
      req(summary_table())
      df <- summary_table()
      
      cat("=====================\n")
      cat("STATISTICAL ANALYSIS\n")
      cat("=====================\n\n")
      
      # Data Characteristics
      cat("DATA CHARACTERISTICS\n")
      cat("-------------------\n")
      cat(df$Value[df$Metric == "Normality"], "\n")
      cat(df$Value[df$Metric == "Variance Equality"], "\n\n")
      
      # Test Information
      cat("TEST SELECTION\n")
      cat("-------------\n")
      cat("Selected Test:", df$Value[df$Metric == "Test Chosen"], "\n\n")
      cat("Reason for Test Selection:\n")
      cat(gsub(", ", "\n- ", df$Value[df$Metric == "Test Reason"]), "\n\n")
      cat("Test Assumptions:\n")
      cat(df$Value[df$Metric == "Test Assumptions"], "\n\n")
      
      # Results
      cat("RESULTS\n")
      cat("-------\n")
      cat("p-value:", df$Value[df$Metric == "p-value"], "\n")
      cat("Significance Level (α):", df$Value[df$Metric == "Significance Level (α)"], "\n")
      cat("Conclusion:", df$Value[df$Metric == "Conclusion"], "\n")
    })
    
    # Download handlers for simplified table
    output$download_summary_txt <- downloadHandler(
      filename = function() {
        paste0("statistical_results_", Sys.Date(), ".txt")
      },
      content = function(file) {
        req(rv$download_table)
        write.table(rv$download_table, file, 
                    sep = "\t", row.names = FALSE, quote = FALSE)
      }
    )
    
    output$download_summary_csv <- downloadHandler(
      filename = function() {
        paste0("statistical_results_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(rv$download_table)
        write.csv(rv$download_table, file, row.names = FALSE)
      }
    )
  
    observeEvent(input$reset_data, {
      rv$column_filtered_data <- NULL
      rv$transformed_data <- NULL
      rv$filtered_data <- NULL
      rv$is_long_format <- FALSE
    })
    
    send_feedback_formspree <- function(message_text) {
      endpoint <- "https://formspree.io/f/mqaqvbzw"
      
      res <- POST(
        url = endpoint,
        body = list(
          message = message_text,
          subject = "Toolkit App Feedback"
        ),
        encode = "form"
      )
      
      if (res$status_code == 200 || res$status_code == 202) {
        return("✅ Feedback sent successfully.")
      } else {
        return(paste("❌ Failed to send feedback. Code:", res$status_code))
      }
    }
    
    observeEvent(input$submit_feedback, {
      feedback <- trimws(input$feedback_text)
      word_count <- length(strsplit(feedback, "\\s+")[[1]])
      
      if (nchar(feedback) == 0) {
        output$feedback_status <- renderText("❗ Feedback cannot be empty.")
        return()
      } else if (word_count > 200) {
        output$feedback_status <- renderText("❗ Feedback exceeds the 200-word limit.")
        return()
      }
      
      send_msg <- tryCatch({
        send_feedback_formspree(feedback)
      }, error = function(e) {
        paste("❌ Email error:", e$message)
      })
      
      output$feedback_status <- renderText(paste(send_msg))
    })
    
    
    
  })
}