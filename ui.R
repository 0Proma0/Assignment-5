library(shiny)


ui = fluidPage(

    titlePanel("Expression in choosen gene"),
    
    sidebarLayout(
        sidebarPanel(
          selectInput("geneID", "Select Gene:", choices = unique(TPMs_table_100genes$GeneID)),
          radioButtons("col", "Select Plot colour:",
                 choices = c("grey","darkblue","darkred","darkorange"),
                 selected = "grey",inline = TRUE)
        ),
        mainPanel(
            plotOutput("Plot")
        )
    )
)
