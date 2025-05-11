library(shiny)
library(tidyverse)
library(DT)
library(UpSetR)

server <- function(input, output, session) {
  # Reactive value to store data
  rv <- reactiveValues(
    raw_data = NULL,
    transformed_data = NULL,
    filtered_data = NULL
  )
  
  # Load data
  observeEvent(input$load_data, {
    req(input$file)
    
    tryCatch({
      ext <- tools::file_ext(input$file$name)
      
      if(input$file_type == "Autodetect") {
        file_type <- ext
      } else {
        file_type <- tolower(input$file_type)
      }
      
      if(file_type %in% c("csv", "tsv", "txt")) {
        sep <- if(file_type == "tsv") "\t" else input$sep
        rv$raw_data <- readr::read_delim(input$file$datapath,
                                         delim = sep,
                                         quote = input$quote,
                                         col_names = input$header)
      } else if(file_type %in% c("xlsx", "xls")) {
        rv$raw_data <- readxl::read_excel(input$file$datapath)
      }
      
      # Update UI elements
      updateSelectizeInput(session, "cols_to_pivot", 
                           choices = names(rv$raw_data),
                           selected = NULL)
      updateSelectInput(session, "names_to", 
                        choices = c("variable", names(rv$raw_data)))
      updateSelectInput(session, "values_to", 
                        choices = c("value", names(rv$raw_data)))
      updateSelectInput(session, "filter_column", 
                        choices = c("None", names(rv$raw_data)))
      
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  # Transform data to long format
  observeEvent(input$transform_data, {
    req(rv$raw_data, input$cols_to_pivot)
    
    tryCatch({
      rv$transformed_data <- rv$raw_data %>%
        tidyr::pivot_longer(cols = input$cols_to_pivot,
                            names_to = input$names_to,
                            values_to = input$values_to)
    }, error = function(e) {
      showNotification(paste("Transformation error:", e$message), type = "error")
    })
  })
  
  # Filter values UI
  output$filter_values_ui <- renderUI({
    req(input$filter_column, input$filter_column != "None")
    col_data <- rv$raw_data[[input$filter_column]]
    
    if(is.numeric(col_data)) {
      sliderInput("filter_range", "Value range",
                  min = min(col_data, na.rm = TRUE),
                  max = max(col_data, na.rm = TRUE),
                  value = range(col_data, na.rm = TRUE))
    } else {
      selectizeInput("filter_values", "Select values",
                     choices = unique(col_data),
                     multiple = TRUE)
    }
  })
  
  # Apply filters
  observeEvent(input$apply_filter, {
    req(rv$raw_data)
    
    filtered <- if(!is.null(rv$transformed_data)) rv$transformed_data else rv$raw_data
    
    # Column filtering
    if(input$filter_column != "None") {
      if(is.numeric(filtered[[input$filter_column]])) {
        req(input$filter_range)
        filtered <- filtered %>% 
          dplyr::filter(between(!!rlang::sym(input$filter_column), 
                                input$filter_range[1], 
                                input$filter_range[2]))
      } else {
        req(input$filter_values)
        filtered <- filtered %>% 
          dplyr::filter(!!rlang::sym(input$filter_column) %in% input$filter_values)
      }
    }
    
    # Row filtering
    if(input$filter_row == "Custom" && nzchar(input$row_range)) {
      rows <- try(eval(parse(text = paste0("c(", input$row_range, ")"))))
                  if(!inherits(rows, "try-error")) {
                    filtered <- filtered %>% dplyr::slice(rows)
                  }
    }
    
    rv$filtered_data <- filtered
  })
      
      # Dynamic plot options UI
      output$plot_options_ui <- renderUI({
        req(rv$raw_data)
        data_to_use <- if(!is.null(rv$filtered_data)) rv$filtered_data else 
          if(!is.null(rv$transformed_data)) rv$transformed_data else rv$raw_data
        
        if(input$plot_type == "UpSet") {
          return(tagList(
            sliderInput("nsets", "Number of sets to show", 
                        min = 2, max = 10, value = 5),
            sliderInput("nintersects", "Number of intersections", 
                        min = 5, max = 50, value = 20)
          ))
        }
        
        tagList(
          switch(input$plot_type,
                 "Scatter" = {
                   tagList(
                     selectInput("x_var", "X Variable", choices = names(data_to_use)),
                     selectInput("y_var", "Y Variable", choices = names(data_to_use)),
                     selectInput("color_var", "Color By", choices = c("None", names(data_to_use)))
                   )
                 },
                 "Bar" = {
                   tagList(
                     selectInput("x_var_bar", "X Variable", choices = names(data_to_use)),
                     selectInput("y_var_bar", "Y Variable", choices = names(data_to_use)),
                     selectInput("fill_var", "Fill By", choices = c("None", names(data_to_use)))
                   )
                 },
                 NULL
          )
        )
      })
      
      # Generate plot
      output$plot <- renderPlot({
        req(input$generate_plot, rv$raw_data)
        
        data_to_use <- if(!is.null(rv$filtered_data)) rv$filtered_data else 
          if(!is.null(rv$transformed_data)) rv$transformed_data else rv$raw_data
        
        tryCatch({
          if(input$plot_type == "UpSet") {
            categorical_cols <- names(data_to_use)[sapply(data_to_use, function(x) is.logical(x) | is.factor(x) | is.character(x))]
            
            if(length(categorical_cols) == 0) {
              return(ggplot2::ggplot() + 
                       ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No categorical columns found for UpSet plot") + 
                       ggplot2::theme_void())
            }
            
            upset_data <- data_to_use %>%
              dplyr::select(dplyr::all_of(categorical_cols)) %>%
              dplyr::mutate(dplyr::across(dplyr::everything(), as.logical))
            
            UpSetR::upset(upset_data, 
                          nsets = input$nsets, 
                          nintersects = input$nintersects)
          } else {
            switch(input$plot_type,
                   "Scatter" = {
                     ggplot2::ggplot(data_to_use, ggplot2::aes_string(x = input$x_var, y = input$y_var)) +
                       ggplot2::geom_point(ggplot2::aes_string(color = if(input$color_var != "None") input$color_var else NULL)) +
                       ggplot2::theme_minimal()
                   },
                   "Bar" = {
                     ggplot2::ggplot(data_to_use, ggplot2::aes_string(x = input$x_var_bar, y = input$y_var_bar)) +
                       ggplot2::geom_col(ggplot2::aes_string(fill = if(input$fill_var != "None") input$fill_var else NULL)) +
                       ggplot2::theme_minimal()
                   },
                   NULL
            )
          }
        }, error = function(e) {
          showNotification(paste("Plot error:", e$message), type = "error")
          NULL
        })
      })
      
      # Data tables
      output$raw_data <- renderDT({
        req(rv$raw_data)
        DT::datatable(rv$raw_data, options = list(scrollX = TRUE))
      })
      
      output$transformed_data <- renderDT({
        req(rv$transformed_data)
        DT::datatable(rv$transformed_data, options = list(scrollX = TRUE))
      })
      
      output$filtered_data <- renderDT({
        req(rv$filtered_data)
        DT::datatable(rv$filtered_data, options = list(scrollX = TRUE))
      })
      
      # Summary
      output$summary <- renderPrint({
        data_to_use <- if(!is.null(rv$filtered_data)) rv$filtered_data else 
          if(!is.null(rv$transformed_data)) rv$transformed_data else rv$raw_data
        req(data_to_use)
        summary(data_to_use)
      })
}