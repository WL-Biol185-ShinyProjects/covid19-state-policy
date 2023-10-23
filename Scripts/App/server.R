# Define server 

server <- function(input, output, session) {
  policyData <- reactive({
    file <- "~/Project/covid19-state-policy/Data/covid19_state_policy_tidydata.csv"
    read.csv(file)
  })
  
  output$policyData <- renderPlot({
    ggplot(aes_string(x = input$x, y = input$y)) +
      geom_point()
  })
}

# Define UI 

ui <- fluidPage(
  
  sidebarLayout(
    
    # Inputs: Select variables to plot
    sidebarPanel(
      
      # Select variable for y-axis
      selectInput(
        inputId = "y",
        label = "Y-axis:",
        choices = c("StringencyIndex", "ContaintmentHealthIndex", "GovernmentResponseIndex", "EconomicSupportIndex"),
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
        inputId = "x",
        label = "Province_State:",
        choices = c(unique(covid19_state_policy_tidydata$Province_State)),
        selected = "Alabama"
      ),
    ),
    
    # Output: Show scatterplot
    mainPanel(
      plotOutput(outputId = "scatterplot")
    )
  )
)



# Create a Shiny app object 

shinyApp(ui = ui, server = server)