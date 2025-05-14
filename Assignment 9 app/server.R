library(shiny)
library(ggplot2)
library(dplyr)

function(input, output, session) {
  longer_TPM <- pivot_longer(TPMs_table_100genes,cols = -c(GeneID),names_to = "Sample",values_to = "TPM")
  
  observe({
    updateSelectInput(session, "geneID", 
                      choices = unique(longer_TPM$GeneID),
                      selected = unique(longer_TPM$GeneID)[1])
  })
  
  output$Plot <- renderPlot({
    data <- subset(longer_TPM, GeneID == input$geneID)
    
    data <- data %>%
      mutate(Condition = ifelse(grepl("Control", Sample), "Control", "Treated"))

    y_upper_limit <- max(data$TPM) * 1.1
    
    base_color <- input$col
    colors <- c("Control" = colorspace::darken(base_color, 0.3),
                "Treated" = colorspace::lighten(base_color, 0.3))
    
    ggplot(data, aes(x = Sample, y = TPM, fill = Condition)) +
      geom_bar(stat = "identity", color = "black") +
      scale_fill_manual(values = colors) +
      labs(title = paste("Expression of gene:", input$geneID),
           x = "Samples",
           y = "TPM") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_y_continuous(limits = c(0, y_upper_limit), expand = c(0, 0))
  })
}