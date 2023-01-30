#pacman::p_load(ggplot2,readxl,shiny,DT,purrr,dplyr,rlang)
ui <- fluidPage(
  actionButton("a","A"),
  selectInput("b","District",choices = unique(df$District)),
  selectInput("c","Parameter",choices = names(df[5:12])),
  tableOutput("z"),
  plotOutput("y")
)

server <- function(input, output, session) {
  
  r0 <- reactive({
    df %>%
      group_by(input$b, year) %>%
      summarise(!!input$c := sum(!!rlang::sym(input$c)))
  })
  
  r1 <- eventReactive(input$a,{r0()})
  
  output$z <- renderTable({
    r1()
    })

  output$y<-plotOutput({
    ggplot(r1(), aes(x = year, y = sum_col, color = input$b)) +
      geom_line() +
      xlab("Year") +
      ylab("Sum of column") +
      ggtitle(paste("Sum of", input$c, "by year and district"))
  })
  
}

shinyApp(ui, server)
