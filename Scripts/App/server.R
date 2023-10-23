library(ggplot2)
library(tidyverse)

# Define server 
file <- "../../Data/covid19_state_policy_tidydata.csv"
policyData <- read.csv(file)
policyData$Converted_Date <- as.Date(policyData$Converted_Date)
policyData <- filter(policyData, policyData$Converted_Date > '2020-04-12')



function(input, output, session) {
  output$indexPlot <- renderPlot({
    
    policyData %>%
      filter(Province_State == input$State) %>%
      ggplot(aes_string(input$x, input$y, group = 1)) +
      geom_point(color='red', fill = 'red', size=1, alpha = 0.5) +
      geom_smooth(color = 'blue') +
      scale_x_date(date_labels="%b %d %Y", 
                   limits = as.Date(c('2020-04-12', '2022-12-31')),
                   breaks = as.Date(c("2020-04-12", "2020-07-01", 
                                      "2021-01-01", "2021-07-01", 
                                      "2022-01-01", "2022-07-01", 
                                      "2022-12-31"))) +
      ylim(0,100)
  })
}