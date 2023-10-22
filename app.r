library(shiny)
library(nycflights13)
library(tidyverse)


ui <- fluidPage(
  titlePanel("NYC Flights"),
  sidebarLayout(
    sidebarPanel = ,
    mainPanel = mainPanel(plotOutput("departureDelayPlot")) 
      
      
      
      selectInput("selectAirport", 
                                      "Airports", 
                                      choices = unique(flights$origin)),
                           sliderInput("slider", 
                                      "# of Bins",
                                      min = 10, 
                                      max = 150, 
                                      value = 20))
    
  )
  selectInput("selectAirport", 
              "Airports", 
              choices = unique(flights$origin)),
  sliderInput("slider", 
              "# of Bins",
              min = 10, 
              max = 150, 
              value = 20),

  plotOutput("departureDelayPlot"))

  
server <- function(input, output) {
  
  output$departureDelayPlot <- renderPlot({
    
    flights %>%
      filter(origin == input$selectAirport) %>%
      ggplot(aes(dep_delay)) + geom_histogram(bins = input$slider)+
      xlim(-50,300)
    
  })
}

shinyApp(ui, server)
