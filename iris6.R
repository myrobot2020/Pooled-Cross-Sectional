library(shiny)

ui <- fluidPage(
  selectInput("model", "Select car model:", 
              choices = unique(mtcars$model), 
              selected = "Mazda RX4"),
  selectInput("col", "Select column to plot against:", 
              choices = c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb"),
              selected = "wt"),
  actionButton("plot_button", "Plot"),
  plotOutput("plot")
)

server <- function(input, output) {
  
  values <- reactiveValues(data = mtcars[mtcars$model == "Mazda RX4", ])
  
  observeEvent(input$plot_button, {
    # Update the reactive values only when the button is pressed
    values$data <- mtcars[mtcars$model == input$model, ]
  })
  
  output$plot <- renderPlot({
    if (is.null(values$data)) return()
    plot(values$data[[input$col]], values$data$mpg)
  })
}

shinyApp(ui, server)
