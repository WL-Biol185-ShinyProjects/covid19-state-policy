# Load packages 

library(shiny)
library(ggplot2)

# Get the data

file <- "https://github.com/WL-Biol185-ShinyProjects/covid19-state-policy/tree/main/Data/"
destfile <- "covid19_state_policy_tidydata.csv"

download.file(file, destfile)

# Load data 

load("~/Project/covid19-state-policy/Data/covid19_state_policy_tidydata.csv")

# Define UI 

ui <- fluidPage(
  
  sidebarLayout(
    
    # Inputs: Select variables to plot
    sidebarPanel(
      
      # Select variable for y-axis
      selectInput(
        inputId = "y",
        label = "Y-axis:",
        choices = c("imdb_rating", "imdb_num_votes", "critics_score", "audience_score", "runtime"),
        selected = "audience_score"
      ),
      # Select variable for x-axis
      selectInput(
        inputId = "x",
        label = "X-axis:",
        choices = c("imdb_rating", "imdb_num_votes", "critics_score", "audience_score", "runtime"),
        selected = "critics_score"
      )
    ),
    
    # Output: Show scatterplot
    mainPanel(
      plotOutput(outputId = "scatterplot")
    )
  )
)

