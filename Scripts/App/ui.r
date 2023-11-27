library(plotly)
library(shinyWidgets)
library(shinythemes)

file <- "../../Data/covid19_state_policy_tidydata.csv"
policyData <- read.csv(file)

lowDensityStates <- c(
  'Alaska',
  'Arizona',
  'Arkansas',
  'Colorado',
  'Idaho',
  'Iowa',
  'Kansas',
  'Kentucky',
  'Louisiana',
  'Maine',
  'Minnesota',
  'Mississippi',
  'Montana',
  'Nebraska',
  'Nevada',
  'New Mexico',
  'North Dakota',
  'Oklahoma',
  'Oregon',
  'South Dakota',
  'Utah',
  'Vermont',
  'West Virginia',
  'Wyoming'
)


mediumDensityStates <- c(
  'Alabama',
  'California',
  'Georgia',
  'Hawaii',
  'Illinois',
  'Indiana',
  'Kentucky',
  'Louisiana',
  'Michigan',
  'Missouri',
  'New Hampshire',
  'North Carolina',
  'South Carolina',
  'Tennessee',
  'Texas',
  'Virginia',
  'Washington',
  'Wisconsin'
)

highDensityStates <- c(
  'Connecticut',
  'Delaware',
  'District of Columbia',
  'Florida',
  'Maryland',
  'Massachusetts',
  'New Jersey',
  'New York',
  'Ohio',
  'Pennsylvania',
  'Rhode Island'
)



# Define UI 

ui <- fluidPage(theme = shinytheme("superhero"),
  
  navbarPage("Covid-19 State Policy Analysis",
             
             tabPanel("Time Series",
                      
                 # Inputs: Select variables to plot
                 sidebarPanel(
                   
                   # Select variable for y-axis
                   selectInput(
                     inputId = "Index",
                     label = "Y-axis:",
                     choices = c("StringencyIndex", "ContainmentHealthIndex", "GovernmentResponseIndex", "EconomicSupportIndex"),
                     selected = "StringencyIndex"
                   ),
                   # # Select variable for x-axis
                   # selectInput(
                   #   inputId = "x",
                   #   label = "X-axis:",
                   #   choices = c("Converted_Date"),
                   #   selected = "Converted_Date"
                   # ),
                   
                   # Select State to filter by
                   selectInput(
                     inputId = "State",
                     label = "State:",
                     choices = c(unique(policyData$Province_State)),
                     selected = "Alabama"),
                   # imageOutput("stateIcon",
                   #             width = "50%",
                   #             height = "40px")
                   ),
                   
                 
                 # Output: Show plots
                 mainPanel(
                   plotOutput("indexPlot"),
                   br(),
                   br(),
                   plotOutput("deathsPlot")
                   )
                 ),
            tabPanel("Cross-Sectional",
                     
                     # CONSIDER USING CONDITONAL PANEL AND WELL PANEL FOR AESTHETIC AND USER INTERFACE
                     
                     wellPanel(
                       sidebarLayout(
                         # Sidebar to demonstrate slider option for time
                         sidebarPanel(
                           checkboxGroupInput("plotType", "Density Categories:",
                                      c("Low Density", "Medium Density", "High Density"),
                                      selected = "Low Density"
                                      ),
                           
                           # Input: Simple time interval
                           sliderInput("Time", "Time:",
                                       min = as.Date("2020-04-13"),
                                       max = as.Date("2022-12-31"),
                                       value = as.Date("2020-04-13"),
                                       animate = animationOptions(interval = 250, loop = TRUE)),
                           
                           # Input: Select Index
                           selectInput(
                             inputId = "IndexTab2",
                             label = "Index",
                             choices = c("StringencyIndex", "ContainmentHealthIndex", "GovernmentResponseIndex", "EconomicSupportIndex"),
                             selected = "StringencyIndex"),
                           
                           # Only show this panel if the plot type is a histogram
                           conditionalPanel(
                             condition = "input.plotType.includes('Low Density')",
                             
                             # Input: Select Low Density States
                             selectInput(
                               multiple = TRUE,
                               inputId = "lowDensityStates",
                               label = "Low Density States",
                               choices = lowDensityStates,
                               selected = lowDensityStates)),
                           
                           # Only show this panel if the plot type is a histogram
                           conditionalPanel(
                             condition = "input.plotType.includes('Medium Density')",
                           
                             # Input: Select Medium Density States
                             selectInput(
                               multiple = TRUE,
                               inputId = "mediumDensityStates",
                               label = "Medium Density States",
                               choices = mediumDensityStates,
                               selected = mediumDensityStates)),
                           
                           # Only show this panel if the plot type is a histogram
                           conditionalPanel(
                             condition = "input.plotType.includes('High Density')",
                             
                             # Input: Select High Density States
                             selectInput(
                               multiple = TRUE,
                               inputId = "highDensityStates",
                               label = "High Density States",
                               choices = highDensityStates,
                               selected = highDensityStates))),
                           
                           mainPanel(
                                     fluidRow(
                                       splitLayout(cellWidths = c("50%", "50%"),
                                                   plotlyOutput(outputId = 'DeathsOverTimebyDensityLow',
                                                                width = "240px",
                                                                height = "480px"),
                                                   plotlyOutput('IndexOverTimeLow',
                                                                height = "455px"))),
                                     br(),
                                     br(),
                                     fluidRow(
                                       splitLayout(cellWidths = c("50%", "50%"),
                                                   plotlyOutput(outputId = 'DeathsOverTimebyDensityMedium',
                                                                width = "240px",
                                                                height = "480px"),
                                                   plotlyOutput('IndexOverTimeMedium',
                                                                height = "450px"))),
                                     br(),
                                     br(),
                                     fluidRow(
                                       splitLayout(cellWidths = c("50%", "50%"),
                                                   plotlyOutput('DeathsOverTimebyDensityHigh',
                                                                width = "240px",
                                                                height = "480px"),
                                                   plotlyOutput('IndexOverTimeHigh',
                                                                height = "450px")))
                           )
                         )
                       )
                     ),
            
            tabPanel("Policy Response Summary",
                     
                     # Inputs: Select variables to plot
                     sidebarPanel(
                       
                       # Select variable for y-axis
                       selectInput(
                         inputId = "StackedIndex",
                         label = "Index:",
                         choices = c("StringencyIndex", "ContainmentHealthIndex", "GovernmentResponseIndex", "EconomicSupportIndex"),
                         selected = "StringencyIndex"
                       )),
                     
                     
                     # Output: Show plots
                     mainPanel(
                               plotOutput("indexStackedPlot",
                                          height = "640px")
                               )
                     ),
            tabPanel("Correlation Matrix",
                     
                     # Inputs: Select variables to plot
                     sidebarPanel(
                       
                       # Select input for switch
                       switchInput(inputId = "colorDisplay",
                                   onLabel = "Color",
                                   offLabel = "Numeric"),
                       
                       # Select state for input
                       selectInput(inputId = "stateForMatrix",
                                   label = "Select State:",
                                   choices = c(lowDensityStates, mediumDensityStates, highDensityStates),
                                   selected = "Alaska")),
                     mainPanel(
                               plotOutput("stateMatrixPlot",
                                          width = "640",
                                          height = "640")
                       
                     )
                     )
            )
  )