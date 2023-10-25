file <- "../../Data/covid19_state_policy_tidydata.csv"
policyData <- read.csv(file)

# Define UI 

ui <- fluidPage(
  
  navbarPage("Covid-19 State Policy Analysis App",
             tabPanel(
               "Component 1",
               
               sidebarLayout(
                 
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
            tabPanel(
              "Component 2",
                        
              sidebarLayout(
                          
                # Inputs: Select variables to plot
                sidebarPanel("Time",
                             "Time:",
                             min = as.Date("2020-04-12","%Y-%m-%d"),
                             max = as.Date("2022-12-31","%Y-%m-%d"),
                             value=as.Date("2020-04-12"),
                             timeFormat="%Y-%m-%d"),
                          
                mainPanel("Plot",
                          plotOutput("DeathsOverTimebyDensity")
                            )
                          ),
            tabPanel("Component 3")
      )         
    )
  )
)
