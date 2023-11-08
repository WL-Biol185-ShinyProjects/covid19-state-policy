library(ggplot2)
library(tidyverse)
library(maps)
library(leaflet)
library(geojsonio)
library(shiny)

# Define server 
file <- "../../Data/covid19_state_policy_tidydata.csv"
policyData <- read.csv(file)
policyData$Converted_Date <- as.Date(policyData$Converted_Date)




function(input, output, session) {
  
  ## TAB 1 [Insert tab title here] ###########################################
  
  output$indexPlot <- renderPlot({
  
    indexPlotTitle <- paste(as.character(input$y), "Over Time — ", as.character(input$State))
    
    policyData %>%
      filter(Province_State == input$State) %>%
      ggplot(aes_string(input$x, input$y, group = 1)) +
      geom_point(color='red', fill = 'red', size=1, alpha = 0.5) +
      geom_smooth(color = 'blue') +
      scale_x_date(date_labels="%b %d %Y", 
                   breaks = as.Date(c("2020-04-12", "2020-07-01", 
                                      "2021-01-01", "2021-07-01", 
                                      "2022-01-01", "2022-07-01", 
                                      "2022-12-31"))) +
      ylim(0,100) +
      ggtitle(indexPlotTitle)+
      theme(plot.title = element_text(hjust = 0.5, size = 22))
  })
    
  output$deathsPlot <- renderPlot({
    
    deathsPlotTitle <- paste("Daily Deaths Over Time — ", as.character(input$State))
  
    policyData %>%
      filter(Province_State == input$State) %>%
      ggplot(aes_string(input$x, 
                        "dailyDeaths", 
                        group = 1)) +
      geom_point(color='red', fill = 'red', size=1, alpha = 0.7) +
      geom_smooth(color = 'blue') +
      xlim(as.Date("2020-04-12"), as.Date("2022-12-31"))+
      scale_x_date(date_labels="%b %d %Y", 
                   breaks = as.Date(c("2020-04-12", "2020-07-01", 
                                      "2021-01-01", "2021-07-01", 
                                      "2022-01-01", "2022-07-01", 
                                      "2022-12-31"))) +
      ylim(0,100) +
      ggtitle(deathsPlotTitle)+
      theme(plot.title = element_text(hjust = 0.5, size = 22))
    
  })
  
  ## TAB 2 [Insert tab title here] ###########################################
  
# output$DeathsOverTimebyDensity <- renderPlot({
#  ggplot(mtcars, aes(mpg, cyl))+
#   geom_point()
# })
    
  
  
  
  
  
  
  
  
  
  
  
    ## TAB 3 Linked Plots Over Maps #########################################
    
#  function getColor(d) {
#    return d > 1000 ? '#800026' :
#      d > 500  ? '#BD0026' :
#      d > 200  ? '#E31A1C' :
#      d > 100  ? '#FC4E2A' :
#      d > 50   ? '#FD8D3C' :
#      d > 20   ? '#FEB24C' :
#      d > 10   ? '#FED976' :
#      '#FFEDA0';
#  }
  
  
    # Setting up index in each state over time
  

    #from chloropleth example repo  
    originalGeo <- geojson_read("../../Data/states.geo.json", what = "sp")
    
    oneDayData <- filter(policyData, policyData$Converted_Date == input$Date)
    
    geo@data <- left_join(originalGeo@data, policyData, by = c("NAME" = "State"))
    
    geo@data <- mutate(geo@data, value = oneDayData, .by = c("NAME" = "State"), .keep = "all")
    

    
    leaflet(geo) %>%
      addPolygons(fillColor = ~pal(value))
    
      indexMapPlotTitle <- paste(as.character(input$y), "Over Time")
   
      
      # Reactive expression for the data subsetted to what the user selected
      filteredData <- reactive({
        policyData[policyData$Date == input$Date,]
      })   
      
      output$indexMap <-renderLeaflet({
        leaflet() %>%
          addProviderTiles(providers$maps,
                           options = providerTileOptions(noWrap = TRUE)
          ) 
          
      })
      
      # observe({
      #   
      #   pal <- colorBin("YlOrRd", domain = geo@data$value)
      #   
      #   leafletProxy("indexMapPlot", data = filteredData()) %>%
      #     clearShapes() %>%
      # 
      # })      
      
         
 




#     
    # Setting up deaths over time in each state on map
      
      deathsMapPlotTitle <- paste("Daily Deaths Over Time")
      
    #   
    # output$deathsMap <- renderLealet({
    #   
    #   
    #   mapStates = map("state", fill = TRUE, plot = FALSE)
    # leaflet(data = mapStates) %>% addTiles() %>%
    #   addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE) 
    # 
    # 
    #  )}

}

