#ui.R

library(shiny)
library(shinydashboard)

source('system1_functions.R')
source('system2_functions.R')

# Local image thing Add directory of static resources to Shiny's web server
duckit = "imgResources/duck_it.png"
addResourcePath(prefix = "imgResources", directoryPath = "images")

# Define UI for application that draws a histogram
shinyUI(
  dashboardPage(
    skin = "blue",
    dashboardHeader(title = "Movies!"),
    dashboardSidebar(disable = TRUE),
    
    dashboardBody(
      fluidRow(
        box(width = 12, title = "Step 1: Rate movies", 
            status = "info", solidHeader = TRUE, collapsible = TRUE,
            uiOutput('ratings'))
      ),#fluidRow 1 ratings
      
      fluidRow(
        # useShinyjs(),
        box(
          width = 12, status = "info", solidHeader = TRUE,
          title = "Step 2: Movies you might like",
          br(),
          actionButton("btn", "Click to get your highly scientific personalized recommendations", class = "btn-warning"),
          br(),
          tableOutput("results")
        )#box
      )#fluidRow 2 results
    )#dashboardBody
  )#dashpoardPage
)#shinyUI
