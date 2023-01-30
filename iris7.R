library(shiny)

ui <- fluidPage(
  selectInput("cyl", "Number of cylinders:", 
              choices = c("4", "6", "8"), 
              selected = "4"),
  selectInput("col", "Select column:", 
              choices = c("mpg", "wt"), 
              selected = "mpg"),
  actionButton("plot_button", "Plot"),
  plotOutput("plot")
)

server <- function(input, output) {
  
  values <- reactiveValues(data = mtcars[mtcars$cyl == 4, c("mpg", "wt")])
  
  observeEvent(input$plot_button, {
    # Update the reactive values only when the button is pressed
    values$data <- mtcars[mtcars$cyl == as.numeric(input$cyl), c(input$col, "wt")]
  })
  
  output$plot <- renderPlot({
    if (is.null(values$data)) return()
    plot(values$data[,1], values$data[,2])
  })
}

shinyApp(ui, server)
