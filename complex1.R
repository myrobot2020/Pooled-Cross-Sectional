pacman::p_load(readxl,shiny,DT,purrr,dplyr)
setwd("C:/Users/asush/Desktop/shinyseason4/Pooled-Cross-Sectional-main")
df <- read_xlsx("gpt6.xlsx")

ui <- fluidPage(
  actionButton("a","A"),
  selectInput("b","District",choices = unique(df$District)),
  selectInput("c","Parameter",choices = names(df[5:12])),
  tableOutput("z")
)

server <- function(input, output, session) {

  
r0<-reactive({
  df %>%
    group_by(input$b, year) %>%
    summarise(input$c = sum(input$c))
  
})

r1<-eventReactive(input$a,{r0()})

output$z<-renderTable(r1())

}

shinyApp(ui, server)


