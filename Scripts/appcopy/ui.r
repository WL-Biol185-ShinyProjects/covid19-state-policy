file <- "../../Data/covid19_state_policy_tidydata.csv"
policyData <- read.csv(file)

library(leaflet)
library(geojsonio)


# Define UI 

ui <- fluidPage(
  
  navbarPage("Covid-19 State Policy App",
             tabPanel(
               "Component 1",
                 
                 # Inputs: Select variables to plot
                 sidebarPanel(
                   
                   # Select variable for y-axis
                   selectInput(
                     inputId = "y",
                     label = "Y-axis:",
                     choices = c("StringencyIndex", "ContainmentHealthIndex", "GovernmentResponseIndex", "EconomicSupportIndex"),
                     selected = "StringencyIndex"
                   ),
                   # Select variable for x-axis
                   selectInput(
                     inputId = "x",
                     label = "X-axis:",
                     choices = c("Converted_Date"),
                     selected = "Converted_Date"
                   ),
                   
                   # Select State to filter by
                   selectInput(
                     inputId = "State",
                     label = "Province_State:",
                     choices = c(unique(policyData$Province_State)),
                     selected = "Alabama"
                   )
                 ),
                 
                 # Output: Show plots
                 mainPanel("Plot",
                            plotOutput("indexPlot"),
                            br(),
                            br(),
                            plotOutput("deathsPlot")
                  )
                ),
               
            tabPanel("Component 2",
                 mainPanel("Plot2",
                           plotOutput("DeathsOverTimebyDensity")
                )
            ),
            
            tabPanel("Component 3",
                     #Inputs: Date slider and Index
                     sidebarPanel(
                       #Select Index
                       selectInput(
                         inputId = "y",
                         label = "Y-axis:",
                         choices = c("StringencyIndex", "ContainmentHealthIndex", "GovernmentResponseIndex", "EconomicSupportIndex"),
                         selected = "StringencyIndex"
                      ),
                       #Date Slider
                      sliderInput("date", "Date", as.Date("2020-04-12"), as.Date("2022-12-31"), as.Date("2020-04-12"), step = NULL, round = FALSE,
                                  ticks = FALSE,
                                  animate = animationOptions(interval = 200, loop = TRUE, playButton = NULL,
                                                                                                         pauseButton = NULL),
                                  width = NULL, sep = ",", pre = NULL, post = NULL, timeFormat = NULL,
                                  timezone = NULL, dragRange = TRUE)
                       
                       
                     ),
                     
                     
                     mainPanel("Plot3",
                               plotOutput("deathsMap"),
                               br(),
                               br(),
                               plotOutput("indexMap")
                               
                     )
             )
            
            
  )         
)

