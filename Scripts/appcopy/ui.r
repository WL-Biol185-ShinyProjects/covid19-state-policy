# file <- "../../Data/covid19_state_policy_tidydata.csv"
# policyData <- read.csv(file)
library(shiny)
library(leaflet)
library(geojsonio)
library(lubridate)


# Define UI 

ui <- fluidPage(
  
  navbarPage("Covid-19 State Policy App",
           
            tabPanel("Component 3",
                     #Inputs: Date slider and Index Options
                     sidebarPanel(
                       #Select Index
                       selectInput(
                         inputId = "selectedIndex",
                         label = "Index: ",
                         choices = list("StringencyIndex" = 1, "ContainmentHealthIndex" = 2, "GovernmentResponseIndex" = 3, "EconomicSupportIndex" = 4),
                         selected = "StringencyIndex"
                      ),
                       #Date Slider
                      sliderInput(inputId = "selectedDay", label = "Date", min = as.Date("2020-04-12"), max = as.Date("2022-12-31"), value = as.Date("2020-04-12"), step = NULL, round = FALSE,
                                  ticks = FALSE,
                                  animate = animationOptions(interval = 400, loop = TRUE, playButton = NULL,
                                                                                                         pauseButton = NULL),
                                  width = NULL, sep = ",", pre = NULL, post = NULL, timeFormat = NULL,
                                  timezone = NULL, dragRange = TRUE)
                       
                       
                     ),
                     
                     
                     mainPanel("Spatial Visualization",
                               leafletOutput("deathsMap"),
                               br(),
                               br(),
                               leafletOutput("indexMap")
                               
                     )
             )
            
            
  )         
)

