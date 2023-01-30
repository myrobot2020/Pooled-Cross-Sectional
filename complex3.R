pacman::p_load(ggplot2,readxl,shiny,DT,purrr,dplyr,rlang,formatR)
setwd("C:/Users/asush/Desktop/shinyseason4/Pooled-Cross-Sectional-main")
df <- read_xlsx("gpt6.xlsx")

ui <- fluidPage(
  actionButton("a","A"),
  selectInput("b","District",choices = unique(df$District),selected = "Kolar"),
  selectInput("c","Parameter",choices = names(df[6:13]),selected = "female_population"),
  tableOutput("z"),
  plotOutput("y")
)

server <- function(input, output, session) {
  
  observeEvent()
  
  
  
  
  r0 <- reactive({
    df %>%
      group_by(District = input$b, year) %>%
      summarise(sum_col = sum(!!rlang::sym(input$c)))
  })
  
  r1 <- eventReactive(input$a,{r0()})
      
  output$z <- renderTable({
    r1()
  })
  
  output$y <- renderPlot({
    ggplot(r1(), aes(x = year, y = sum_col, color = District)) +
      geom_point() +
      xlab("Year") +
      ylab("Sum of column") +
      isolate(ggtitle(paste("Sum of", input$c, "by year and district")))
  })
  
}

shinyApp(ui, server)
