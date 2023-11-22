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
  
  #LOW
  
  policyDataLow <- reactive({
      policyData %>%
        filter(Province_State %in% input$lowDensityStates & Converted_Date == input$Time) 
  })
  
  output$DeathsOverTimebyDensityLow <- renderPlotly({
    
    deathsDensityPlotTitle <- paste("Daily Deaths Over Time by State sorted by Density")
     
    densityData <- if (input$Time <= as.Date("2020-07-01")){                                   #THIS NEEDS CORRECTION. ASSIGN THE VARIABLE OUTSIDE OF THE IF ELSE BLOCK
      densityData[order(densityData$basePD),]
    }else if(input$Time <= as.Date("2021-07-01")){
      arrange(densityData, densityData$`2020PD`)
    }else if(input$Time <= as.Date("2022-07-01")){
      arrange(densityData, densityData$`2021PD`)
    } else{
      arrange(densityData, densityData$`2022PD`)
    }
     
    densityOrder <- densityData$State
    
    policyData$Province_State <- factor(policyData$Province_State, levels = densityOrder)
    
    #Setting up low density states df
    lowStates <- filter(densityData, densityData$State %in% input$lowDensityStates)
    lowStatesVector <- lowStates$State

    plot_ly(data = policyDataLow(),
            x = ~Province_State, 
            y = ~dailyDeaths,
            type = 'bar',
            width = 800, 
            height = 450) %>%
      layout(xaxis = list(title = input$lowDensityStates),
             yaxis = list(title =list(text = 'Daily Deaths', standoff = 10), 
             range = c(0,250)))
  })
  
  policyDataLowIndex <- reactive({
    policyData %>%
      filter(Province_State %in% input$lowDensityStates & Converted_Date <= input$Time) 
  })
  
  output$IndexOverTimeLow <- renderPlotly({
    
    #Setting up low density states
    
    # lowStates <- filter(densityData, densityData$State %in% input$lowDensityStates)
    # lowStatesVector <- lowStates$State
    # 
    # 
    # 
    # policyDataLow <- policyData %>%
    #   filter(Province_State %in% lowStatesVector) %>%
    #   filter(Converted_Date <= input$Time) %>%
      
    plot_ly(
      data = policyDataLowIndex(),
      x = ~Converted_Date, 
      y = as.formula(paste0('~',input$IndexTab2)),
      split = ~Province_State,
      type = 'scatter',
      mode = 'lines'
      ) %>%
      layout(xaxis = list(title = "Time", range = c(as.Date("2020-04-12"), as.Date("2022-12-31"))),
             yaxis = list(title = as.character(input$IndexTab2), range = c(0,100)))
    
  })
  
  # MEDIUM
  
  policyDataMedium <- reactive({
    policyData %>%
      filter(Province_State %in% input$mediumDensityStates & Converted_Date == input$Time) 
  })
  
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
    
    #Setting up medium density states plot

    plot_ly(data = policyDataMedium(),
            x = ~Province_State,
            y = ~dailyDeaths,
            type = 'bar',
            width = 800,
            height = 450,
            color = 'orange') %>%
      layout(xaxis = list(title = input$mediumDensityStates),
             yaxis = list(title =list(text = 'Daily Deaths', standoff = 10),
                          range = c(0,500)))

  
  #   #Setting up medium density states df
  #   mediumStates <- filter(densityData, densityData$class == "Medium")
  #   densityOrderMedium <- intersect(densityOrder, mediumStates$State)
  #   policyDataMedium <- policyData %>% 
  #     filter(Province_State %in% mediumStates$State)
  #   
  #   policyDataMedium %>%
  #     filter(Converted_Date == input$Time) %>%
  #     arrange(densityOrderMedium) %>%
  #     plot_ly(x = ~Province_State, 
  #             y = ~dailyDeaths,
  #             type = 'bar',
  #             width = 800, 
  #             height = 450,
  #             color = 'orange') %>%
  #     layout(xaxis = list(title=list(text = "Medium Density States", standoff = 10)),
  #            yaxis = list(title=list(text = 'Daily Deaths', standoff = 10), 
  #                         range = c(0,500)))
  })
  
  policyDataMediumIndex <- reactive({
    policyData %>%
      filter(Province_State %in% input$mediumDensityStates & Converted_Date <= input$Time) 
  })
  
  output$IndexOverTimeMedium <- renderPlotly({
    
    #Setting up medium density states index
    
    plot_ly(
      data = policyDataMediumIndex(),
      x = ~Converted_Date, 
      y = as.formula(paste0('~',input$IndexTab2)),
      split = ~Province_State,
      type = 'scatter',
      mode = 'lines'
    ) %>%
      layout(xaxis = list(title = "Time", range = c(as.Date("2020-04-12"), as.Date("2022-12-31"))),
             yaxis = list(title = as.character(input$IndexTab2), range = c(0,100)))
    
  })
  
  # HIGH
  
  output$DeathsOverTimebyDensityHigh <- renderPlotly({
    
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
    lowStates <- filter(densityData, densityData$class == "High")
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
      layout(xaxis = list(title=list(text = "High Density States", standoff = 10)),
             yaxis = list(title=list(text = 'Daily Deaths', standoff = 10), 
                          range = c(0,1000)))
    
  })
  
  output$IndexOverTimeHigh <- renderPlotly({
    
  #Setting up high density states
  highStates <- filter(densityData, densityData$class == "High")
  policyDataLow <- policyData %>%
    filter(Province_State %in% highStates$State) %>%
    filter(Converted_Date <= input$Time) %>%
    plot_ly(
      x = ~Converted_Date, 
      y = as.formula(paste0('~', input$IndexTab2)),
      split = ~Province_State,
      # frame = ~frame,
      type = 'scatter',
      mode = 'lines'
      # line = list(color = "red")
    ) %>%
    layout(xaxis = list(title = "Time", range = c(as.Date("2020-04-12"), as.Date("2022-12-31"))),
           yaxis = list(title = as.character(input$IndexTab2), range = c(0,100)))
  })
  
  # Observer Block
  
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
    plotlyProxy("IndexOverTimeMedium", session) %>%
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
    plotlyProxy("IndexOverTimeHigh", session) %>%
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
}
