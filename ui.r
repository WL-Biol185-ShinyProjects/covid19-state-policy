library(plotly)
library(shinyWidgets)
library(shinythemes)
library(shiny)
library(leaflet)
library(geojsonio)
library(lubridate)

file <- "Data/covid19_state_policy_tidydata.csv"
file2 <- "Data/OxCGRT_USA_detailed_policy_tidy.csv"
policyData <- read.csv(file)
detailedPolicyData <- read.csv(file2)

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

ui <- fluidPage(theme = shinytheme("darkly"),
  
  navbarPage("Covid-19 State Policy Analysis",
             
             tabPanel("Time Series",
                      
                 # Inputs: Select variables to plot
                 sidebarPanel(
                   
                   # Select variable for y-axis
                   selectInput(
                     inputId = "Index",
                     label = "Index:",
                     choices = c("StringencyIndex", "ContainmentHealthIndex", "GovernmentResponseIndex", "EconomicSupportIndex"),
                     selected = "StringencyIndex"
                   ),
                   
                   # Select State to filter by
                   selectInput(
                     inputId = "State",
                     label = "State:",
                     choices = c(unique(policyData$Province_State)),
                     selected = "Alabama"),
                   p("This linked plot enables users to compare
                     trends in state policy response and mortalities
                     across the period of data collection. Users
                     may select from the states and the computed 
                     OxCGRT indeces (range 0-100)."),
                   p("Description of the Policy Indeces:"),
                   em("1. StringencyIndex:"),
                   p("Documents the severity of closure and containment measures implemented
                     that restricted population movement and behavior."),
                   em("2. ContainmentHealthIndex:"),
                   p("Record the extent and intensity of efforts to contain Covid-19 
                   and protect public health encompassing closure and containment measures
                   along with health protocols like Covid-19 testing rate and contact tracing)."),
                   em("3. GovernmentResponseIndex:"),
                   p("Reports the variability in state policy response across all 
                     documented measures."),
                   em("4. EconomicSupportIndex:"),
                   p("Reflects the degree economic support made available
                     to a specific state at a point in time, including (but
                     not limited to) income assistance and debt forgiveness.")
                   ),
                   
                 
                 # Output: Show plots
                 mainPanel(
                   h3("Time Series"),
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
                           p("These linked plots enables users to examine
                     trends in state policy response and mortalities
                     from a cross-section perspective. The states are 
                           categorized by density. The user may select
                           the specific states and index to visualize 
                           from the options below."),
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
                           
                           mainPanel(h3("Cross Sectional"),
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
            tabPanel("Policy Response - Detailed",
                     mainPanel(h3("Policy Response - Detailed"),
                               # Select variable for y-axis
                               selectInput(
                                 "stateMeasureRepresentation",
                                 label = h5("Select Policy Measure:"),
                                 multiple = FALSE,
                                 choices = c(colnames(detailedPolicyData)[4:19]),
                                 selected = "C1M_School.closing"
                               ),
                               
                               p("The first visualization ranks US states by the duration of time spent at > 2 for a given OxCGRT policy measure.
                       Thus, states at the bottom of the table may be considered 'Most Restrictive', while states towards the top may
                       be considered 'Least Restrictive' in policy response. Users may toggle between the various policy measures above."),
                               
                               plotOutput("policyMeasureStackedPlot",
                                          height = "640px"),
                               p("Source: Oxford Covid-19 Government Response Tracker", align = "center"),
                               br(),
                               selectInput(
                                 inputId = "stateCompare",
                                 label = "State:",
                                 choices = c(lowDensityStates, mediumDensityStates, highDensityStates),
                                 selected = "Alaska"
                               ),
                               p("The second visualization provides a state-specific summary of the various OxCGRT policy measures
                               across the data collection period. Users may select the state to be visualized using the dropdown above."),
                               plotOutput("policyMeasureStatePlot",
                                          height = "640px"),
                               br(),
                               br()
                     )
                     
            ),
            
            tabPanel("Policy Response - Summary",
               
                     # Output: Show plots
                     mainPanel(h3("Policy Response Summary"),
                               # Select variable for y-axis
                               selectInput(
                                 inputId = "StackedIndex",
                                 label = "Index:",
                                 choices = c("StringencyIndex", "ContainmentHealthIndex", "GovernmentResponseIndex", "EconomicSupportIndex"),
                                 selected = "StringencyIndex"
                               ),
                               
                               p("The first visualization ranks US states by the duration of time spent at > 60 for a given OxCGRT policy index (range 0-100).
                       Thus, states at the top of the table may be considered 'Most Restrictive', while states towards the bottom may
                       be considered 'Least Restrictive' in policy response. Users may toggle between the four policy indeces above."),
                               plotOutput("indexStackedPlot",
                                          height = "640px"),
                               p("Source: Oxford Covid-19 Government Response Tracker", align = "center"),
                               br(),
                               selectInput(
                                 "stateIndexRepresentation",
                                 label = h5("Select State:"),
                                 multiple = FALSE,
                                 choices = c(lowDensityStates, 
                                             mediumDensityStates,
                                             highDensityStates),
                                 selected = "Alaska"
                               ),
                               p("The second visualization provides a state-specific summary of the 4 OxCGRT policy indeces (range 0-100)
                               across the data collection period. Users may select the state to be visualized using the dropdown above."),
                               plotOutput("indexStatePlot",
                                          height = "640px"),
                               br(),
                               br()
                               )
                     ),
            
            tabPanel("Correlation Matrix",
                     
                     # Inputs: Select variables to plot
                     sidebarPanel(
                       
                       # Select input for switch
                       p("P-Value / Correlation Value Toggle:"),
                       switchInput(inputId = "colorDisplay",
                                   onLabel = "p-Value",
                                   offLabel = "r-Value"),
                       
                       # Select state for input
                       selectInput(inputId = "stateForMatrix",
                                   label = "Select State:",
                                   choices = c(lowDensityStates, mediumDensityStates, highDensityStates),
                                   selected = "Alaska"),
                       p("This visualization helps users explore the significance of the relationships
                         between state policy responses and mortality outcomes for the given period
                         of data collection. Correlations were computed by corrplot with an alpha
                         threshold of 0.05.")
                       ),
                     mainPanel(h3("Correlation Matrix"),
                               plotOutput("stateMatrixPlot",
                                          width = "640",
                                          height = "640")
                       
                     )
                     ),
            tabPanel("Linear Regression",
                     # Inputs: Select variables to plot
                     sidebarPanel(
                       # sliderInput(
                       #   "Slider1",
                       #   label = h4("Train/Test Split %"),
                       #   min = 0,
                       #   max = 100,
                       #   value = 90
                       # ),
                       # textOutput("cntTrain"),
                       # textOutput("cntTest"),
                       
                       selectInput(
                         "predictors",
                         label = h5("Select Predictors:"),
                         multiple = FALSE,
                         choices = c("StringencyIndex",
                                     "ContainmentHealthIndex",
                                     "GovernmentResponseIndex",
                                     "EconomicSupportIndex"
                                     ),
                         selected = "StringencyIndex"
                         ),
                       # selectInput(
                       #   "predictorState",
                       #   label = h5("Select State:"),
                       #   multiple = FALSE,
                       #   choices = c(lowDensityStates, 
                       #               mediumDensityStates,
                       #               highDensityStates
                       #   ),
                       #   selected = "Alaska"
                       # ),
                       selectInput(
                         "predictOutput",
                         label = h5("Variable to Predict:"),
                         choices = c("dailyDeaths")
                       ),
                       p("This visualization helps users explore the potential linear regression relationships
                         between state policy responses and mortality outcomes for the given period
                         of data collection.")
                       ),
                     mainPanel(
                         h4("Linear Regression"),
                         plotOutput("multiRegressionPlot"),
                         verbatimTextOutput("prediction")
                       )
              
            ),
            tabPanel("Chloropleth",
                     #Inputs: Date slider and Index Options
                     sidebarPanel(
                       #Select Index
                       selectInput(
                         inputId = "selectedIndex",
                         label = "Index: ",
                         choices = c("StringencyIndex", "ContainmentHealthIndex", "GovernmentResponseIndex", "EconomicSupportIndex"),
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
                     
                     
                     mainPanel("Plot3",
                               leafletOutput("deathsMap"),
                               br(),
                               br(),
                               leafletOutput("indexMap")
                               )
                     ),
            tabPanel("Appendix",
                     h2("Appendix"),
                     br(),
                     h4("The 4 Oxford Covid-19 Government Reponse Tracker Policy Indeces:"),
                     em("1. StringencyIndex:"),
                     p("Documents the severity of closure and containment measures implemented
                     that restricted population movement and behavior."),
                     em("2. ContainmentHealthIndex:"),
                     p("Record the extent and intensity of efforts to contain Covid-19 
                   and protect public health encompassing closure and containment measures
                   along with health protocols like Covid-19 testing rate and contact tracing)."),
                     em("3. GovernmentResponseIndex:"),
                     p("Reports the variability in state policy response across all 
                     documented measures."),
                     em("4. EconomicSupportIndex:"),
                     p("Reflects the degree economic support made available
                     to a specific state at a point in time, including (but
                     not limited to) income assistance and debt forgiveness."),
                     br(),
                     h4("Index Methodology"),
                     p("Users interested in further details of the index methodology may read below:"),
                     a("OxCGRT Index Methodology", href = "https://github.com/OxCGRT/covid-policy-tracker/blob/master/documentation/index_methodology.md"),
                     br(),
                     br(),
                     h4("Accessing the Raw Data"),
                     p("Users interested in accessing the raw data from the OxCGRT may do so here:"),
                     a("OxCGRT - United States - Sub-National Data", href = "https://github.com/OxCGRT/covid-policy-tracker/tree/master/data/United%20States")
                     )
            )
  )