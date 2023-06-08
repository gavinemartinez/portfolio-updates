library(shiny)
library(ggplot2)



# Define UI for application
ui <- fluidPage(
  
  # Text input for p-value
  textInput("pval", "Enter p-value:"),
  
  # Text input for non-centrality parameter
  textInput("ncp", "Enter non-centrality parameter:"),
  
  textInput('df', "Enter degrees of freedom:"),
  
  # Output plot
  plotOutput("tplot"),
)

server <- function(input, output) {
  output$tplot <- renderPlot({
    
    pval <- as.numeric(input$pval)
    ncp <- as.numeric(input$ncp)
    df <- as.numeric(input$df)
    
    # Calculate critical value
    crit_val <- qt(pval/2, df, ncp)
    
    # Generate t distribution plot
    seq <- seq(-4, 4, length.out = 100)
    s = data.frame(x = seq,
                   y = dt(seq, df, ncp))
    #plot(x, y, type = "l", main = "T Distribution on {df$} DF", xlab = "T Value", ylab = "Density")
    ggplot(s, aes(x = x, y = y)) +
      geom_line(aes(x = x, y = y)) +
      geom_vline(xintercept = crit_val, color = "red") +
      annotate("text", x = crit_val, y = dt(crit_val, df, ncp), 
               label = paste0("CV = ", round(crit_val, 2)), 
               vjust = -0.5, hjust = -0.5, size = 4, color = "red") +
      labs(title = sprintf("T Distribution with %s DF", input$df),
           subtitle = sprintf("Non-Centrality Parameter: %s", input$ncp),
           x = "T",
           y = "density") +
      theme_classic() +
      theme(plot.title = element_text(size = 20))
    
    # Add critical value to plot
    #abline(v = crit_val, col = "red")
    #text(crit_val + 0.2, 0.25, round(crit_val, 2), pos = 4, col = "red")
  })
}

shinyApp(ui = ui, server = server)