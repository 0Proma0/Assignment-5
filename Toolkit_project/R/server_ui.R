library(shiny)
library(httr)
library(jsonlite)
library(DT)
library(ggplot2)
library(colourpicker)

blast_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      proteins <- reactiveVal(NULL)
      fasta_seq <- reactiveVal(NULL)
      blast_results <- reactiveVal(NULL)
      
      # ---- Helper functions ----
      search_uniprot <- function(protein, organism) {
        base_url <- "https://rest.uniprot.org/uniprotkb/search"
        query <- paste0(protein, " AND organism_name:", organism)
        res <- GET(url = base_url, query = list(
          query = query,
          format = "json",
          fields = "accession,id,protein_name,organism_name",
          size = 10
        ))
        if (status_code(res) != 200) return(NULL)
        data <- fromJSON(content(res, "text", encoding = "UTF-8"), simplifyDataFrame = FALSE)
        entries <- data$results
        if (is.null(entries) || length(entries) == 0) return(NULL)
        setNames(
          sapply(entries, function(x) x$primaryAccession),
          sapply(entries, function(x) {
            pname <- tryCatch(x$proteinDescription$recommendedName$fullName$value, error = function(e) "No name")
            org <- tryCatch(x$organism$scientificName, error = function(e) "No organism")
            paste0(pname, " [", org, "]")
          }))
      }
      
      get_fasta_from_uniprot <- function(uniprot_id) {
        url <- paste0("https://rest.uniprot.org/uniprotkb/", uniprot_id, ".fasta")
        res <- GET(url)
        if (status_code(res) != 200) return("Error downloading FASTA.")
        content(res, "text", encoding = "UTF-8")
      }
      
      submit_blast <- function(fasta_seq) {
        url <- "https://www.ebi.ac.uk/Tools/services/rest/ncbiblast/run"
        res <- POST(
          url,
          body = list(
            sequence = fasta_seq,
            program = "blastp",
            database = "uniprotkb_swissprot",
            email = "tweronika.lepiarz@student.uj.edu.pl",
            stype = "protein"
          ),
          encode = "form"
        )
        if (status_code(res) != 200) return(NULL)
        content(res, "text", encoding = "UTF-8")
      }
      
      check_blast_status <- function(job_id) {
        url <- paste0("https://www.ebi.ac.uk/Tools/services/rest/ncbiblast/status/", job_id)
        res <- GET(url)
        if (status_code(res) != 200) return(NULL)
        content(res, "text", encoding = "UTF-8")
      }
      
      get_blast_result <- function(job_id) {
        url <- paste0("https://www.ebi.ac.uk/Tools/services/rest/ncbiblast/result/", job_id, "/json")
        res <- GET(url)
        if (status_code(res) != 200) return(NULL)
        content(res, "text", encoding = "UTF-8")
      }
      
      parse_blast_json <- function(json_text) {
        result <- tryCatch(fromJSON(json_text), error = function(e) NULL)
        if (is.null(result)) return(NULL)
        if (!is.null(result$hits) && nrow(result$hits) > 0) {
          hits <- result$hits
          n <- nrow(hits)
          Identity <- rep(NA, n)
          E_value  <- rep(NA, n)
          Bit_score <- rep(NA, n)
          Align_len <- rep(NA, n)
          Query_from <- rep(NA, n)
          Query_to   <- rep(NA, n)
          Hit_from   <- rep(NA, n)
          Hit_to     <- rep(NA, n)
          for (i in seq_len(n)) {
            hsp_df <- hits$hit_hsps[[i]]
            if (is.null(hsp_df)) next
            if (!is.data.frame(hsp_df) || nrow(hsp_df) < 1) next
            Identity[i]   <- if ("hsp_identity" %in% names(hsp_df) && "hsp_align_len" %in% names(hsp_df)) round(100 * hsp_df$hsp_identity[1] / hsp_df$hsp_align_len[1], 2) else NA
            E_value[i]    <- if ("hsp_expect" %in% names(hsp_df)) hsp_df$hsp_expect[1] else NA
            Bit_score[i]  <- if ("hsp_bit_score" %in% names(hsp_df)) hsp_df$hsp_bit_score[1] else NA
            Align_len[i]  <- if ("hsp_align_len" %in% names(hsp_df)) hsp_df$hsp_align_len[1] else NA
            Query_from[i] <- if ("hsp_query_from" %in% names(hsp_df)) hsp_df$hsp_query_from[1] else NA
            Query_to[i]   <- if ("hsp_query_to" %in% names(hsp_df)) hsp_df$hsp_query_to[1] else NA
            Hit_from[i]   <- if ("hsp_hit_from" %in% names(hsp_df)) hsp_df$hsp_hit_from[1] else NA
            Hit_to[i]     <- if ("hsp_hit_to" %in% names(hsp_df)) hsp_df$hsp_hit_to[1] else NA
          }
          df <- data.frame(
            Hit_ID      = hits$hit_id,
            Accession   = hits$hit_acc,
            Description = hits$hit_desc,
            Organism    = hits$hit_os,
            Gene        = hits$hit_uni_gn,
            Length      = hits$hit_len,
            Identity    = Identity,
            E_value     = E_value,
            Bit_score   = Bit_score,
            Align_len   = Align_len,
            Query_from  = Query_from,
            Query_to    = Query_to,
            Hit_from    = Hit_from,
            Hit_to      = Hit_to,
            URL         = hits$hit_url,
            stringsAsFactors = FALSE
          )
          return(df)
        }
        return(NULL)
      }
      
      # ---- Server logic ----
      observeEvent(input$search, {
        result <- search_uniprot(input$protein_name, input$organism)
        if (is.null(result)) {
          showNotification("No results found.", type = "error")
          proteins(NULL)
        } else {
          proteins(result)
        }
      })
      
      output$protein_select_ui <- renderUI({
        req(proteins())
        selectInput(ns("protein_id"), "Select protein:", choices = proteins())
      })
      
      observeEvent(input$get_fasta, {
        if (!is.null(input$fasta_file)) {
          ext <- tools::file_ext(input$fasta_file$name)
          if (tolower(ext) != "fasta") {
            showNotification("Only .fasta files are allowed.", type = "error")
            return()
          }
          fasta <- paste(readLines(input$fasta_file$datapath), collapse = "\n")
          fasta_seq(fasta)
          output$fasta_output <- renderText(fasta)
        } else if (!is.null(input$protein_id)) {
          fasta <- get_fasta_from_uniprot(input$protein_id)
          fasta_seq(fasta)
          output$fasta_output <- renderText(fasta)
        } else {
          showNotification("Select a protein or upload a file.", type = "warning")
        }
      })
      
      output$download_fasta <- downloadHandler(
        filename = function() "protein.fasta",
        content = function(file) {
          writeLines(fasta_seq(), file)
        }
      )
      
      observeEvent(input$run_blast, {
        req(fasta_seq())
        output$blast_status <- renderUI({ h4("Running BLAST... please wait.") })
        withProgress(message = "Running BLAST...", value = 0, {
          job_id <- submit_blast(fasta_seq())
          if (is.null(job_id)) {
            output$blast_status <- renderUI({ h4("Error submitting BLAST job.") })
            return()
          }
          for (i in 1:45) {
            Sys.sleep(2)
            status <- check_blast_status(job_id)
            incProgress(1/45, detail = paste("Checking status (", i * 2, "s):", status))
            if (status == "FINISHED") break
            if (status %in% c("ERROR", "FAILURE")) {
              output$blast_status <- renderUI({ h4("BLAST failed.") })
              return()
            }
          }
          json_result <- get_blast_result(job_id)
          if (is.null(json_result)) {
            output$blast_status <- renderUI({ h4("Failed to get result.") })
            blast_results(NULL)
            return()
          }
          df <- parse_blast_json(json_result)
          if (is.null(df) || nrow(df) == 0) {
            output$blast_status <- renderUI({ h4("No hits found in BLAST result.") })
            blast_results(NULL)
            return()
          }
          updateSliderInput(session, "length_range", min = min(df$Length, na.rm = TRUE), max = max(df$Length, na.rm = TRUE), value = c(min(df$Length, na.rm = TRUE), max(df$Length, na.rm = TRUE)))
          updateSliderInput(session, "identity_range", min = 0, max = 100, value = c(0, 100))
          e_min <- suppressWarnings(log10(min(df$E_value[df$E_value > 0], na.rm = TRUE)))
          e_max <- suppressWarnings(log10(max(df$E_value[df$E_value > 0], na.rm = TRUE)))
          if (!is.finite(e_min)) e_min <- -100
          if (!is.finite(e_max)) e_max <- 0
          updateSliderInput(session, "evalue_range", min = e_min, max = e_max, value = c(e_min, e_max))
          blast_results(df)
          output$blast_status <- renderUI({
            tags$div(style="color: green; font-weight:bold;", "BLAST finished! Check the Results Table and Plots tabs.")
          })
          invalidateLater(5000, session)
          observeEvent(invalidateLater(5000, session), {
            output$blast_status <- renderUI({ NULL })
          }, once = TRUE)
        })
      })
      
      output$download_results <- downloadHandler(
        filename = function() "blast_results.csv",
        content = function(file) {
          df <- blast_results()
          if (!is.null(df)) write.csv(df, file, row.names = FALSE)
        }
      )
      
      filtered_results <- reactive({
        df <- blast_results()
        req(df)
        df <- df[!is.na(df$Length) & df$Length >= input$length_range[1] & df$Length <= input$length_range[2], ]
        df <- df[is.na(df$Identity) | (df$Identity >= input$identity_range[1] & df$Identity <= input$identity_range[2]), ]
        if (any(!is.na(df$E_value))) {
          evalue_log <- suppressWarnings(log10(df$E_value))
          df <- df[is.na(evalue_log) | (evalue_log >= input$evalue_range[1] & evalue_log <= input$evalue_range[2]), ]
        }
        if (!is.null(input$columns_to_show)) {
          df <- df[, input$columns_to_show, drop = FALSE]
        }
        df
      })
      
      output$blast_table <- renderDT({
        req(filtered_results())
        datatable(filtered_results(), options = list(pageLength = 10), filter = "top")
      })
      
      output$plot_settings <- renderUI({
        req(blast_results())
        df <- filtered_results()
        num_vars <- names(df)[sapply(df, is.numeric)]
        if (input$plot_type == "hist") {
          selectInput(ns("hist_var"), "Select variable for histogram:", choices = num_vars)
        } else if (input$plot_type == "scatter") {
          tagList(
            selectInput(ns("scatter_x"), "X axis:", choices = num_vars, selected = num_vars[1]),
            selectInput(ns("scatter_y"), "Y axis:", choices = num_vars, selected = num_vars[min(2, length(num_vars))])
          )
        } else if (input$plot_type == "boxplot") {
          selectInput(ns("box_var"), "Select variable for boxplot:", choices = num_vars)
        }
      })
      
      plot_reactive <- reactive({
        df <- filtered_results()
        req(df)
        if (nrow(df) < 1) return(NULL)
        fill_col <- input$plot_fill
        border_col <- input$plot_border
        border_size <- input$plot_border_size
        point_size <- input$point_size
        if (input$plot_type == "hist") {
          var <- input$hist_var
          req(var)
          ggplot(df, aes_string(x = var)) +
            geom_histogram(bins = input$plot_bins, fill = fill_col, color = border_col, size = border_size) +
            theme_minimal()
        } else if (input$plot_type == "scatter") {
          req(input$scatter_x, input$scatter_y)
          ggplot(df, aes_string(x = input$scatter_x, y = input$scatter_y)) +
            geom_point(color = fill_col, size = point_size) +
            theme_minimal()
        } else if (input$plot_type == "boxplot") {
          var <- input$box_var
          req(var)
          ggplot(df, aes_string(y = var)) +
            geom_boxplot(fill = fill_col, color = border_col, size = border_size) +
            theme_minimal()
        }
      })
      
      output$blast_plot <- renderPlot({
        plot_reactive()
      })
      
      output$download_plot <- downloadHandler(
        filename = function() paste0("blast_plot_", input$plot_type, ".png"),
        content = function(file) {
          ggsave(file, plot = plot_reactive(), width = 7, height = 5)
        }
      )
    }
  )
}