library(ggplot2)
library(tidyverse)
library(plotly)
library(purrr)
library(corrplot)
library(caret)
library(reshape2)


# Define server 
file1 <- "Data/covid19_state_policy_tidydata.csv"
policyData <- read.csv(file1)
policyData$Converted_Date <- as.Date(policyData$Converted_Date)

file2 <- "Data/state-population-density-data.csv"
densityData <- read.csv(file2)


function(input, output, session) {
  
  observeEvent(once = TRUE,ignoreNULL = FALSE, ignoreInit = FALSE, eventExpr = input$click, { 
    
    showModal(modalDialog(
      title = "Welcome Page", 
      h1('Covid-19 US State Policy Analysis App', align = "center"),
      p("Authors: Sanil Partha '25 & Sarp Sahin '26", align = "center"),
      p(" Using OxCGRT indicators and indices, we visualized variation in US state 
      policy responses to the Covid-19 pandemic from April 2020 to December 2022. We 
      subsequently compared OxCGRT indices alongside case-fatality data from the JHU 
      Covid-19 Data Repository to evaluate the effectiveness of policy-driven response
        on Covid-19 related mortalities."),
      p("References:"),
      a("1. Oxford COVID-19 Government Response Tracker, Blavatnik School of Government, University of Oxford, https://github.com/OxCGRT/covid-policy-tracker.",
        href = "https://github.com/OxCGRT/covid-policy-tracker"),
      a("2. COVID-19 Data Repository by the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University",
        href = "https://github.com/CSSEGISandData/COVID-19")
    ))
  })
  
  ## TAB 1 [Time Series] ###########################################
  
  output$indexPlot <- renderPlot({
  
    indexPlotTitle <- paste(as.character(input$Index), "Over Time — ", as.character(input$State))

    policyData %>%
      filter(Province_State == input$State) %>%
      ggplot(aes_string("Converted_Date", input$Index, group = 1)) +
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
      ggplot(aes_string("Converted_Date", 
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
  
  ## TAB 2 [Cross-Sectional] ###########################################
  
  #LOW
  
  policyDataLow <- reactive({
      policyData %>%
        filter(Province_State %in% input$lowDensityStates & Converted_Date == input$Time) 
  })
  
  output$DeathsOverTimebyDensityLow <- renderPlotly({
    
    deathsDensityPlotTitle <- paste("Daily Deaths Over Time by State sorted by Density")
     
    densityData <- if (input$Time <= as.Date("2020-07-01")){                                   
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
      layout(xaxis = list(title=list(text = "Low Density States", standoff = 10)),
             yaxis = list(title=list(text = 'Daily Deaths', standoff = 10), 
                          range = c(0,250)))

  })
  
  policyDataLowIndex <- reactive({
    policyData %>%
      filter(Province_State %in% input$lowDensityStates & Converted_Date <= input$Time) 
  })
  
  output$IndexOverTimeLow <- renderPlotly({
  
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
      layout(xaxis = list(title=list(text = "Medium Density States", standoff = 10)),
                    yaxis = list(title=list(text = 'Daily Deaths', standoff = 10),
                          range = c(0,500)))

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
      type = 'scatter',
      mode = 'lines'
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
  })
  
  ## TAB 3 [Policy Response - Detailed] ###########################################
  
  output$indexStackedPlot <- renderPlot({
    
    # Plot Title
    indexStackedPlotTitle <- paste("Time Periods States spent under", 
                                   as.character(input$StackedIndex), 
                                   "(Source: OxCRGT)")
    
    # Collapse Table on Index Duration > 60 
    
    indexDuration <- policyData %>%
      group_by(Province_State) %>%
      summarize(indexDurationVector = sum(!!rlang::sym(input$StackedIndex) > 60, na.rm = TRUE)) %>%
      arrange(desc(indexDurationVector))
    
    # Obtain Duration Vector
    stateVector <- indexDuration$Province_State
    
    # Color scale for index levels
    color_scale <- c("0-20" = "#fcfdbf", 
                     "20-40" = "#fc8961", 
                     "40-60" = "#b73779", 
                     "60-80" = "#51127c", 
                     "80-100" = "#000004")
    
    # Breaks and labels for legend
    indexBreaks <- c(0, 20, 40, 60, 80, 100)
    indexLabels <- c("0-20", "20-40", "40-60", "60-80", "80-100")
    
    # Reorder States by State Vector
    policyData$Province_State <- factor(policyData$Province_State, levels = stateVector)
    
    # Plot output
    ggplot(policyData, aes(x = Converted_Date,
                 y = Province_State,
                 fill = cut(.data[[input$StackedIndex]],
                            breaks = indexBreaks,
                            labels = indexLabels))) +
      geom_tile()+
      scale_fill_manual(values = color_scale, na.value = "white")+
      ggtitle(indexStackedPlotTitle)+
      labs(x = "Time", 
           y = "States",
           fill= paste(as.character(input$StackedIndex)))+
      theme(plot.title = element_text(hjust = 0.5, size = 18),
            axis.text=element_text(size=12),
            axis.title = element_text(size=16),
            legend.text = element_text(size=14),
            title = element_text(size = 24),
            legend.title = element_text(size = 16))
  })
  
  output$indexStatePlot <- renderPlot({
    
    # Plot Title
    indexStatePlotTitle <- paste("Index Averages for", 
                                   as.character(input$stateIndexRepresentation))
    
    # Filter by State
    filteredData <- policyData %>%
      filter(Province_State == input$stateIndexRepresentation) %>%
      filter(Converted_Date <= '2022-12-31' & Converted_Date >= '2020-04-13') %>%
      select(c("Converted_Date", "StringencyIndex", "ContainmentHealthIndex", "GovernmentResponseIndex", "EconomicSupportIndex"))
    
    filteredDataMelted <- melt(filteredData,  id.vars = 'Converted_Date', variable.name = 'indeces')
    
    
    # Plot output
    ggplot(filteredDataMelted, aes(x = Converted_Date,
                           y = value))+
      geom_line(aes(color = indeces))+
      ylim(0,100)+
      labs(title = indexStatePlotTitle,
           x = "Time",
           y = "Index",
           color = "Index Type")+
      theme(axis.text=element_text(size=12),
            axis.title = element_text(size=16),
            title = element_text(size = 18),
            legend.text = element_text(size=14)
            )

  })
  
  ## TAB 4 [Policy Response - Summary] ###########################################
  
  output$indexStackedPlot <- renderPlot({
    
    # Plot Title
    indexStackedPlotTitle <- paste("Time Periods States spent under", 
                                   as.character(input$StackedIndex), 
                                   "(Source: OxCRGT)")
    
    # Collapse Table on Index Duration > 60 
    
    indexDuration <- policyData %>%
      group_by(Province_State) %>%
      summarize(indexDurationVector = sum(!!rlang::sym(input$StackedIndex) > 60, na.rm = TRUE)) %>%
      arrange(desc(indexDurationVector))
    
    # Obtain Duration Vector
    stateVector <- indexDuration$Province_State
    
    # Color scale for index levels
    color_scale <- c("0-20" = "#fcfdbf", 
                     "20-40" = "#fc8961", 
                     "40-60" = "#b73779", 
                     "60-80" = "#51127c", 
                     "80-100" = "#000004")
    
    # Breaks and labels for legend
    indexBreaks <- c(0, 20, 40, 60, 80, 100)
    indexLabels <- c("0-20", "20-40", "40-60", "60-80", "80-100")
    
    # Reorder States by State Vector
    policyData$Province_State <- factor(policyData$Province_State, levels = stateVector)
    
    # Plot output
    ggplot(policyData, aes(x = Converted_Date,
                           y = Province_State,
                           fill = cut(.data[[input$StackedIndex]],
                                      breaks = indexBreaks,
                                      labels = indexLabels))) +
      geom_tile()+
      scale_fill_manual(values = color_scale, na.value = "white")+
      ggtitle(indexStackedPlotTitle)+
      labs(x = "Time", 
           y = "States",
           fill= paste(as.character(input$StackedIndex)))+
      theme(plot.title = element_text(hjust = 0.5, size = 18),
            axis.text=element_text(size=12),
            axis.title = element_text(size=16),
            legend.text = element_text(size=14),
            title = element_text(size = 24),
            legend.title = element_text(size = 16))
  })
  
  output$indexStatePlot <- renderPlot({
    
    # Plot Title
    indexStatePlotTitle <- paste("Index Averages for", 
                                 as.character(input$stateIndexRepresentation))
    
    # Filter by State
    filteredData <- policyData %>%
      filter(Province_State == input$stateIndexRepresentation) %>%
      filter(Converted_Date <= '2022-12-31' & Converted_Date >= '2020-04-13') %>%
      select(c("Converted_Date", "StringencyIndex", "ContainmentHealthIndex", "GovernmentResponseIndex", "EconomicSupportIndex"))
    
    filteredDataMelted <- melt(filteredData,  id.vars = 'Converted_Date', variable.name = 'indeces')
    
    
    # Plot output
    ggplot(filteredDataMelted, aes(x = Converted_Date,
                                   y = value))+
      geom_line(aes(color = indeces))+
      ylim(0,100)+
      labs(title = indexStatePlotTitle,
           x = "Time",
           y = "Index",
           color = "Index Type")+
      theme(axis.text=element_text(size=12),
            axis.title = element_text(size=16),
            title = element_text(size = 18),
            legend.text = element_text(size=14)
      )
    
  })
  
  
  ## TAB 4 [Correlation Matrix] ###########################################
  
  output$stateMatrixPlot <- renderPlot({
    
    # Plot Title
    stateMatrixPlotTitle <- paste(as.character(input$stateForMatrix), 
                                   "Correlation Matrix")
    
    # # Collapse Table on Index Duration > 60
    # indexDuration <- policyData %>%
    #   group_by(Province_State) %>%
    #   summarize(indexDurationVector = sum(!!rlang::sym(input$StackedIndex) > 60, na.rm = TRUE)) %>%
    #   arrange(desc(indexDurationVector))
    # 
    # # Obtain Duration Vector
    # stateVector <- indexDuration$Province_State
    # 
    # # Color scale for index levels
    # color_scale <- c("<20" = "#fcfdbf", 
    #                  "20-40" = "#fc8961", 
    #                  "40-60" = "#b73779", 
    #                  "60-80" = "#51127c", 
    #                  "80-100" = "#000004")
    # 
    # # Breaks and labels for legend
    # indexBreaks <- c(0, 20, 40, 60, 80, 100)
    # indexLabels <- c("<20", "20-40", "40-60", "60-80", "80-100")
    # 
    # # Reorder States by State Vector
    # policyData$Province_State <- factor(policyData$Province_State, levels = stateVector)
    
    # Plot output
    
    matrixData <- policyData %>%
      filter(Province_State == as.character(input$stateForMatrix))
    
    matrixData <- subset(matrixData, select = c(StringencyIndex,
                               ContainmentHealthIndex,
                               GovernmentResponseIndex,
                               EconomicSupportIndex,
                               Incident_Rate,
                               dailyDeaths,
                               Case_Fatality_Ratio
                               ))
    
    M <- cor(matrixData, use="pairwise.complete.obs")
    testRes = cor.mtest(M, conf.level = 0.95)
    
    
    if (input$colorDisplay) {
      corrplot(M, method = 'number',
               addCoef.col = "grey50",
               tl.col = 'black',
               diag = FALSE) # numeric
      } else {
        corrplot(M, 
                 p.mat = testRes$p,
                 method = 'color',
                 # sig.level = c(0.001, 0.01, 0.05), 
                 # pch.cex = 2,
                 pch.col = 'white',
                 insig = 'p-value',
                 sig.level = -1,
                 tl.col = 'black',
                 diag = FALSE) # colorful number  
    }
    
  })
  
  ## TAB 5 [Linear Regression] ###########################################
  
  output$prediction <- renderPrint({
    
    filteredData <- policyData %>%
      filter(Converted_Date <= '2022-12-31' & Converted_Date >= '2020-04-13')

    # Obtain inputted predictive variables
    varPredict <- c(input$predictors)

    # Develop linear model
    lmformula <- as.formula(paste(input$predictOutput, " ~ ", paste(varPredict, collapse = " + ")))
    multiRegression <- lm(lmformula, data = filteredData)
    summary(multiRegression)
  })
  
  output$multiRegressionPlot <- renderPlot({
    
    # Obtain inputted predictive variables
    varPredict <- c(input$predictors)
    
    # Filter Data based on State and Date
    filteredData <- policyData %>%
      filter(Converted_Date <= '2022-12-31' & Converted_Date >= '2020-04-13')
    
    filteredData[filteredData == 0] <- NA

    
    # Develop linear model
    lmformula <- as.formula(paste(input$predictOutput, " ~ ", paste(varPredict, collapse = " + ")))
    multiRegression <- lm(lmformula, data = filteredData, na.action=na.omit)

    ggplot(data = filteredData,
           aes_string(x = input$predictors,
                      y = input$predictOutput))+
      geom_point(aes(color = Province_State))+
      geom_smooth(method = "lm")+
      theme(axis.text=element_text(size=12),
            axis.title = element_text(size=16),
            title = element_text(size = 18),
            legend.text = element_text(size = 12))+
      labs(x = paste(input$predictors),
           y = paste(input$predictOutput),
           title = paste(input$predictors,"vs", input$predictOutput),
           color = "State")

  })
  
  
}
