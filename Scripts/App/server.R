library(ggplot2)
library(tidyverse)
library(plotly)
library(purrr)

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
  
  output$DeathsOverTimebyDensityLow <- renderPlotly({
    
    deathsDensityPlotTitle <- paste("Daily Deaths Over Time by State sorted by Density")
     
    if (input$Time <= as.Date("2020-07-01")){
      densityData <- densityData[order(densityData$basePD),]
    }else if(input$Time <= as.Date("2021-07-01")){
       densityData <- arrange(densityData, densityData$`2020PD`)
    }else if(input$Time <= as.Date("2022-07-01")){
       densityData <- arrange(densityData, densityData$`2021PD`)
    } else{
       densityData <- arrange(densityData, densityData$`2022PD`)
    }
     
    densityOrder <- densityData$State
    
    policyData$Province_State <- factor(policyData$Province_State, levels = densityOrder)
    
    #Setting up low density states df
    lowStates <- filter(densityData, densityData$class == "Low")
    densityOrderLow <- intersect(densityOrder, lowStates$State)
    policyDataLow <- policyData %>% 
      filter(Province_State %in% lowStates$State)
    
    policyDataLow %>%
      filter(Converted_Date == input$Time) %>%
      arrange(densityOrderLow) %>%
      plot_ly(x = ~Province_State, 
              y = ~dailyDeaths,
              type = 'bar',
              width = 800, 
              height = 450) %>%
      layout(xaxis = list(title=list(text = "Low Density States", standoff = 10)),
             yaxis = list(title=list(text = 'Daily Deaths', standoff = 10), 
                          range = c(0,1000)))
    
  })
  
  output$IndexOverTimeLow <- renderPlotly({
    
    #Setting up low density states
    
    lowStates <- filter(densityData, densityData$class == "Low")
    policyDataLow <- policyData %>%
      filter(Province_State %in% lowStates$State) %>%
      filter(Converted_Date <= input$Time) %>%
      plot_ly(
        x = ~Converted_Date, 
        y = as.formula(paste0('~', input$Index)),
        split = ~Province_State,
        # frame = ~frame,
        type = 'scatter',
        mode = 'lines'
        # line = list(color = "red")
      ) %>%
      layout(xaxis = list(title = "Time", range = c(as.Date("2020-04-12"), as.Date("2022-12-31"))),
             yaxis = list(title = "Index", range = c(0,100)))
    
  })
  
  ##WORK ON THIS NEXT
  
  observe({
    plotlyProxy("IndexOverTimeLow", session) %>%
      plotlyProxyInvoke(
        "addTraces", 
        list(
          x = input$Time,
          y = input$Index,
          type = "scatter",
          mode = "lines",
          line = list(color = "red")
        )
      )
    }
  )

  output$DeathsOverTimebyDensityMedium <- renderPlotly({
    
    deathsDensityPlotTitle <- paste("Daily Deaths Over Time by State sorted by Density")
    
    if (input$Time <= as.Date("2020-07-01")){
      densityData <- densityData[order(densityData$basePD),]
    }else if(input$Time <= as.Date("2021-07-01")){
      densityData <- arrange(densityData, densityData$`2020PD`)
    }else if(input$Time <= as.Date("2022-07-01")){
      densityData <- arrange(densityData, densityData$`2021PD`)
    } else{
      densityData <- arrange(densityData, densityData$`2022PD`)
    }
    
    densityOrder <- densityData$State
    
    policyData$Province_State <- factor(policyData$Province_State, levels = densityOrder)
    
    #Setting up medium density states df
    mediumStates <- filter(densityData, densityData$class == "Medium")
    densityOrderMedium <- intersect(densityOrder, mediumStates$State)
    policyDataMedium <- policyData %>% 
      filter(Province_State %in% mediumStates$State)
    
    policyDataMedium %>%
      filter(Converted_Date == input$Time) %>%
      arrange(densityOrderMedium) %>%
      plot_ly(x = ~Province_State, 
              y = ~dailyDeaths,
              type = 'bar',
              width = 800, 
              height = 450,
              color = 'orange') %>%
      layout(xaxis = list(title=list(text = "Medium Density States", standoff = 10)),
             yaxis = list(title=list(text = 'Daily Deaths', standoff = 10), 
                          range = c(0,1000)))
  })
}
    # p3 <- policyData %>%
    #   filter(Converted_Date == input$Time) %>%
    #   arrange(densityOrder) %>%
    #   plot_ly(x = ~Province_State, 
    #           y = ~dailyDeaths,
    #           type = 'bar',
    #           width = 800, 
    #           height = 450) %>%
    #   layout(xaxis = list(title=list(text = "State", standoff = 10)),
    #          yaxis = list(title=list(text = 'Daily Deaths', standoff = 10), 
    #                       range = c(0,1000)))
    # 
