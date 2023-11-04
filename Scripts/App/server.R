library(ggplot2)
library(tidyverse)
library(plotly)

# Define server 
file1 <- "../../Data/covid19_state_policy_tidydata.csv"
policyData <- read.csv(file1)
policyData$Converted_Date <- as.Date(policyData$Converted_Date)

file2 <- "../../Data/state-population-density-data.csv"
densityData <- read.csv(file2)


function(input, output, session) {
  
  ## TAB 1 [Insert tab title here] ###########################################
  
  output$indexPlot <- renderPlot({
  
    indexPlotTitle <- paste(as.character(input$Index), "Over Time — ", as.character(input$State))

    policyData %>%
      filter(Province_State == input$State) %>%
      ggplot(aes_string(input$x, input$Index, group = 1)) +
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
      geom_smooth(method = "loess", color = 'blue', span = .25) +
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
  
  ## TAB 2 [Deaths Over Time by State; sorted by Density] ###########################################
  
  output$DeathsOverTimebyDensity <- renderPlotly({
    
    deathsDensityPlotTitle <- paste("Daily Deaths Over Time by State sorted by Density")
    
    policyData$Province_State <- as.factor(policyData$Province_State)
    
    policyData %>%
      filter(Converted_Date == input$Time) %>%
      plot_ly(x = ~Province_State, 
              y = ~dailyDeaths,
              type = 'bar') %>%
      layout(xaxis = list(type = 'category', title=list(text = "State", standoff = 10)),
             yaxis = list(title=list(text = 'Daily Deaths', standoff = 10), range = c(0,1000)))
    
  })
}