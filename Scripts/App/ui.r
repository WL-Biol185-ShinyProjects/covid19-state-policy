file <- "../../Data/covid19_state_policy_tidydata.csv"
policyData <- read.csv(file)

# Define UI 

ui <- fluidPage(
  
  navbarPage("Covid-19 State Policy App",
             tabPanel(
               "Component 1",
                 
                 # Inputs: Select variables to plot
                 sidebarPanel(
                   
                   # Select variable for y-axis
                   selectInput(
                     inputId = "Index",
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
                      # Sidebar to demonstrate slider option for time
                      sidebarPanel(
                        
                        # Select State to filter by
                        # selectInput(
                        #   inputId = "State",
                        #   label = "Province_State:",
                        #   choices = c(unique(policyData$Province_State)),
                        #   selected = "Alabama"
                        # ),
                        
                        # Input: Simple time interval
                        sliderInput("Time", "Time:",
                                    min = as.Date("2020-04-13"), max = as.Date("2022-12-31"),
                                    value = as.Date("2020-04-13"), animate =
                                      animationOptions(interval = 200, loop = TRUE)),
                
                      mainPanel("Plot2",
                           plotOutput("DeathsOverTimebyDensity")))
    )                  
  )         
)

