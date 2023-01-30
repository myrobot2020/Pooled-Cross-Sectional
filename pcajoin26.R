library(shiny)
library(dplyr)

ui <- fluidPage(
  selectInput("a","levels",choices = c("District","Subdistrict","NULL")),
  # selectInput("b","variable",choices = c("male_population","female_population","male_literate_population",
  #                                        "female_literate_population","male_sc_population","female_sc_population",
  #                                        "male_st_population","female_st_population")),
  tableOutput("x")
)

server <- function(input, output, session) {
  r1<-reactive({
    df<-read_xlsx("gpt4.xlsx")
    df<-

  output$x<-renderTable({
    r1()
  })
  
}

shinyApp(ui, server)
