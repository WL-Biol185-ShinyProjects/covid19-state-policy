library(ggplot2)
library(tidyverse)
library(plotly)
library(purrr)
library(corrplot)
library(caret)

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
      p("Authors: Sanil Partha '26 & Sarp Sahin '26", align = "center"),
      p(" Using OxCGRT indicators and indices, we visualized variation in US state 
      policy responses to the Covid-19 pandemic from April 2020 to December 2022. We 
      subsequently compared OxCGRT indices alongside case-fatality data from the JHU 
      Covid-19 Data Repository to evaluate the effectiveness of policy-driven response
        on Covid-19 related mortalities."),
      p("References:"),
      p("1. Oxford COVID-19 Government Response Tracker, Blavatnik School of Government, University of Oxford, https://github.com/OxCGRT/covid-policy-tracker."),
      p("2. COVID-19 Data Repository by the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University, https://github.com/CSSEGISandData/COVID-19")
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
    
    if (("Low Density") %in% input$plotType) {
      plot_ly(data = policyDataLow(),
              x = ~Province_State, 
              y = ~dailyDeaths,
              type = 'bar',
              width = 800, 
              height = 450) %>%
        layout(xaxis = list(title=list(text = "Low Density States", standoff = 10)),
               yaxis = list(title=list(text = 'Daily Deaths', standoff = 10), 
                            range = c(0,250)))
    } else {
      # Do not output anything
    }

  })
  
  policyDataLowIndex <- reactive({
    policyData %>%
      filter(Province_State %in% input$lowDensityStates & Converted_Date <= input$Time) 
  })
  
  output$IndexOverTimeLow <- renderPlotly({
  
    if (("Low Density") %in% input$plotType) {
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
    } else {
      # Do not output anything
    }
    
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
    
    if (("Medium Density") %in% input$plotType) {
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
    } else {
      # Do not output anything
    }

  })
  
  policyDataMediumIndex <- reactive({
    policyData %>%
      filter(Province_State %in% input$mediumDensityStates & Converted_Date <= input$Time) 
  })
  
  output$IndexOverTimeMedium <- renderPlotly({
    
  #Setting up medium density states index
  if (("Medium Density") %in% input$plotType) {
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
    } else {
      # Do not output anything
    }  
    
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
    
    if (("High Density") %in% input$plotType) {
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
    } else {
      # Do not output anything
    }
    
    
  })
  
  output$IndexOverTimeHigh <- renderPlotly({
    
  #Setting up high density states
      
  highStates <- filter(densityData, densityData$class == "High")
  
  if (("High Density") %in% input$plotType) {
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
  } else {
    # Do not output anything
  }
  
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
  
  ## TAB 3 [Policy Response Summary] ###########################################
  
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
    color_scale <- c("<20" = "#fcfdbf", 
                     "20-40" = "#fc8961", 
                     "40-60" = "#b73779", 
                     "60-80" = "#51127c", 
                     "80-100" = "#000004")
    
    # Breaks and labels for legend
    indexBreaks <- c(0, 20, 40, 60, 80, 100)
    indexLabels <- c("<20", "20-40", "40-60", "60-80", "80-100")
    
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
      theme(plot.title = element_text(hjust = 0.5, size = 22),
            axis.text=element_text(size=11))
  })
  
  ## TAB 4 [Correlation Matrix] ###########################################
  
  output$stateMatrixPlot <- renderPlot({
    
    # Plot Title
    stateMatrixPlotTitle <- paste(as.character(input$stateForMatrix), 
                                   "Correlation Matrix")
    
    # Collapse Table on Index Duration > 60
    indexDuration <- policyData %>%
      group_by(Province_State) %>%
      summarize(indexDurationVector = sum(!!rlang::sym(input$StackedIndex) > 60, na.rm = TRUE)) %>%
      arrange(desc(indexDurationVector))
    
    # Obtain Duration Vector
    stateVector <- indexDuration$Province_State
    
    # Color scale for index levels
    color_scale <- c("<20" = "#fcfdbf", 
                     "20-40" = "#fc8961", 
                     "40-60" = "#b73779", 
                     "60-80" = "#51127c", 
                     "80-100" = "#000004")
    
    # Breaks and labels for legend
    indexBreaks <- c(0, 20, 40, 60, 80, 100)
    indexLabels <- c("<20", "20-40", "40-60", "60-80", "80-100")
    
    # Reorder States by State Vector
    policyData$Province_State <- factor(policyData$Province_State, levels = stateVector)
    
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
      # If the switch is OFF, don't render the plot
        corrplot(M, 
                 p.mat = testRes$p,
                 method = 'color',
                 sig.level = c(0.001, 0.01, 0.05), pch.cex = 2,
                 insig = 'label_sig', pch.col = 'white',
                 tl.col = 'black',
                 diag = FALSE) # colorful number  
    }
    
  })
  
  ## TAB 5 [Predictive Modeling] ###########################################
  
  # output$prediction <- renderPrint({
  #   
  #   filteredData <- policyData %>%
  #     filter(Province_State == input$predictorState)
  #   
  #   # Obtain inputted predictive variables
  #   varPredict <- c(input$predictors)
  #   
  #   # Develop linear model
  #   lmformula <- as.formula(paste(input$predictOutput, " ~ ", paste(varPredict, collapse = " + ")))
  #   multiRegression <- lm(lmformula, data = filteredData)
  #   summary(multiRegression)
  # })
  
  output$prediction <- renderPrint({
    
    filteredData <- policyData %>%
      filter(Province_State == input$predictorState) %>%
      filter(Converted_Date <= '2022-12-31' & Converted_Date >= '2020-04-13')

    # numRowsToKeep <- round(nrow(filteredData) * (as.integer(input$Slider1)/100))
    # 
    # TrainingData <- filteredData %>%
    #   slice(1:numRowsToKeep)
    # 
    # TestData <- filteredData %>%
    #   slice(numRowsToKeep:nrow(filteredData))

    # Obtain inputted predictive variables
    varPredict <- c(input$predictors)

    # Develop linear model
    lmformula <- as.formula(paste(input$predictOutput, " ~ ", paste(varPredict, collapse = " + ")))
    multiRegression <- lm(lmformula, data = filteredData)
    # predictedValues <- predict(multiRegression, newdata = TestData, interval = 'confidence')
    summary(multiRegression)
  })
  
  output$multiRegressionPlot <- renderPlot({
    
    # Obtain inputted predictive variables
    varPredict <- c(input$predictors)
    
    # Filter Data based on State and Date
    filteredData <- policyData %>%
      filter(Province_State == input$predictorState) %>%
      filter(Converted_Date <= '2022-12-31' & Converted_Date >= '2020-04-13')
    
    # # Create Time Slices for LM Model
    # numRowsToKeep <- round(nrow(filteredData) * (as.integer(input$Slider1)/100))
    # 
    # timeSlices <- createTimeSlices(filteredData$Converted_Date,
    #                           initialWindow = numRowsToKeep,
    #                           horizon = nrow(filteredData) - numRowsToKeep)
    # 
    # # Create Training Data and Test Data
    # trainingIndex <- timeSlices$train[[1]]
    # testingIndex <- timeSlices$test[[1]]
    # 
    # trainingData <- filteredData[trainingIndex,]
    # testingData <- filteredData[testingIndex,]
    
    # # Setting up trainControl for time series cross-validation
    # ctrl <- trainControl(method = "timeslice", index = timeSlices)
    # 
    # 
    # # Creating the lm model
    # lmformula <- as.formula(paste(input$predictOutput, " ~ ", paste(varPredict, collapse = " + ")))
    # multiRegression <- train(lmformula, data = trainingData, method = "lm", na.action = na.pass)
    # 
    # predictedValues <- predict(multiRegression, newdata = testingData)
    
    # Develop linear model
    lmformula <- as.formula(paste(input$predictOutput, " ~ ", paste(varPredict, collapse = " + ")))
    multiRegression <- lm(lmformula, data = filteredData)
    # predictedValues <- predict(multiRegression, newdata = testingData)
    
    # , interval = 'confidence')

    ggplot(data = filteredData,
           aes_string(x = input$predictors,
                      y = input$predictOutput))+
      geom_point()+
      geom_smooth(method = "lm")+
      theme(axis.text=element_text(size=12),
            axis.title = element_text(size=16),
            title = element_text(size = 18))+
      labs(x = paste(input$predictors),
           y = paste(input$predictOutput),
           title = paste(input$predictors," vs ", input$predictOutput))
    
    par(mfrow=c(2,2))
    plot(multiRegression)

  })
  
  
}
