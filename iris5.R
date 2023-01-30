library(shiny)

ui <- fluidPage(
  selectInput("species", "Select species:", 
              choices = c("setosa", "versicolor", "virginica"), 
              selected = "setosa"),
  actionButton("plot_button", "Plot"),
  plotOutput("plot")
)

server <- function(input, output) {
  
  values <- reactiveValues(data = iris[iris$Species == "setosa", ])
  
  observeEvent(input$plot_button, {
    # Update the reactive values only when the button is pressed
    values$data <- iris[iris$Species == input$species, ]
  })
  
  output$plot <- renderPlot({
    if (is.null(values$data)) return()
    plot(values$data$Petal.Length, values$data$Petal.Width)
  })
}

shinyApp(ui, server)
