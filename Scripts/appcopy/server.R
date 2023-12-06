library(ggplot2)
library(tidyverse)
library(maps)
library(leaflet)
library(geojsonio)
library(shiny)
library(readr)
library(dplyr)
library(RColorBrewer)


# Define server 
file <- "../../Data/covid19_state_policy_tidydata.csv"

setwd("/home/parthas25@ad.wlu.edu/covid19-state-policy/Scripts/appcopy")

policyData <- read.csv(file)
policyData$Converted_Date <- as.Date(policyData$Converted_Date)

print(getwd())



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
    
  
  
  
  
  
  
  
  
  
  
  
    ## TAB 3 Linked Plots Over Maps 
    
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
  # Reactive expression for the data subsetted to what the user selected
    geo <- geojson_read("../../Data/states.geo.json", what = "sp")

    deathsGeo <- geo
    indexGeo <- geo
    
    
    
    bins <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
    deathsPal <- colorNumeric(palette = "YlOrRd", domain = min(policyData$dailyDeaths) : max(policyData$dailyDeaths), na.color = "white")
    indexPal <- colorNumeric(palette = "YlOrRd", domain = 0:100, na.color = "white")
    
    
   
    #observer block updating spatial file for deaths map with data for selected day on input slider
   
    observe({
      dayDeathsData <- filter(policyData[,c("Province_State", "dailyDeaths", "Converted_Date")]
                              , input$selectedDay == policyData$Converted_Date) 
      dayDeathsData$NAME <- dayDeathsData$Province_State      #matches name of state column in geo@data
      
      
      deathsGeo@data <- left_join(geo@data, dayDeathsData, by = "NAME")
      
    })
    
    #observer block updating spatial file for index map with data for selected day on input slider
    
    observe({
      dayIndexData <- filter(policyData[,c("Province_State", input$selectedIndex, "Converted_Date")]
                      , input$selectedDay == policyData$Converted_Date)
      
      
      
      dayIndexData$NAME <- dayIndexData$Province_State
      indexGeo@data <- left_join(geo@data, dayIndexData, by = "NAME")
      
      
    })
    
    
    
    
    
    
    output$deathsMap <- renderLeaflet({
      leaflet(deathsGeo) %>%
        addTiles()%>%
          addPolygons(
            fillColor = ~deathsPal(deathsGeo@data$dailyDeaths),
              weight = 2,
                opacity = 1,
                  color = "gray",
                    dashArray = "3",
                      fillOpacity = 0.7)
      })
      
    
    
    observe({
    leafletProxy("deathsMap", data = deathsGeo@data)
    })
      
      
    
    output$indexPlot <- renderLeaflet({
      leaflet(indexGeo) %>%
        addTiles()%>%
          addPolygons( #add data argument
           fillColor = ~indexPal(output$selectedIndex),
             weight = 2,
              opacity = 1,
               color = "gray",
                dashArray = "3",
                  fillOpacity = 0.7)
      })
    observe({
      leafletProxy("indexMap", data = indexGeo@data)
    })
    
    
   

}