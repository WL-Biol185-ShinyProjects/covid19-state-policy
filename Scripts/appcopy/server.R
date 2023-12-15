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

#print(getwd())



function(input, output, session) {
  
    ## TAB 3

    # Setting up index in each state over time
  

  #from chloropleth example repo  
  # Reactive expression for the data subsetted to what the user selected
    geo <- geojson_read("../../Data/states.geo.json", what = "sp")

    deathsGeo <- geo
    indexGeo <- geo
    
    
   
    #observer block updating spatial file for deaths map with data for selected day on input slider
   
    deathsData <- policyData[,c("Province_State", "dailyDeaths", "Converted_Date")]
   
     observe({
      
      dayDeathsData <- filter(DeathsData, input$selectedDay == deathsData$Converted_Date) 
        
      dayDeathsData$NAME <- dayDeathsData$Province_State      #matches name of state column in geo@data
      
      
      deathsGeo@data <- left_join(geo@data, dayDeathsData, by = "NAME")
      
    })
    
    #observer block updating spatial file for index map with data for selected day on input slider
    
    observe({
      dayIndexData <- policyData[,
                        c("Province_State", 
                            "StringencyIndex",
                              "GovernmentResponseIndex", 
                                "EconomicSupportIndex", 
                                  "ContainmentHealthIndex",
                                    "Converted_Date"
                          )
                      ]
        filter(dayIndexData, input$selectedDay == dayIndexData$Converted_Date)



      dayIndexData$NAME <- dayIndexData$Province_State
      indexGeo@data <- left_join(geo@data, dayIndexData, by = "NAME")


    })
    
    
# Color Palettes
    
  deathsPal <- colorBin(
                    "YlOrRd",
                      domain = deathsData$dailyDeaths
              )
    
  indexPal <- colorBin(
                  "YlOrRd",
                    domain = 0:100
              )
    
    
# Daily Death Choropleth
    
    output$deathsMap <- renderLeaflet({
      leaflet(deathsGeo) %>%
        addTiles()%>%
          setView(-100, 40, zoom = 3)
      })
      
    
    
    observe({
    leafletProxy("deathsMap", data = deathsGeo@data)%>%
        addPolygons(data = deathsGeo,
          fillColor = ~deathsPal(deathsGeo@data$dailyDeaths)
            # weight = 2,
            #   opacity = 1,
            #     color = "gray",
            #       dashArray = "3",
            #         fillOpacity = 0.7
          )
    })

      
    # output$indexMap <- renderLeaflet({
    #   leaflet(indexGeo) %>%
    #     addTiles()%>%
    #       setView(-100, 40, zoom = 3)
    #   })
    # observe({
    #   leafletProxy("indexMap", data = indexGeo@data)%>%
    # addPolygons(
    #   fillColor = ~indexPal(indexGeo@data$indexCol),
    #     weight = 2,
    #       opacity = 1,
    #         color = "gray",
    #           dashArray = "3",
    #             fillOpacity = 0.7)
    # })

}