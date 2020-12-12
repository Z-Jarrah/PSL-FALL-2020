#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)

# library(shinyjs)

# Local image thing Add directory of static resources to Shiny's web server
duckit = "imgResources/duck_it.png"
addResourcePath(prefix = "imgResources", directoryPath = "images")

# Define UI for application that draws a histogram
shinyUI(
  dashboardPage(
    # Application title
    # titlePanel("Old Faithful Geyser Data"),
    
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
          uiOutput('results')
          # tableOutput("results")
        )
      )#fluidRow 2 results
    )#dashboardBody
  )#dashpoardPage
)#shinyUI
