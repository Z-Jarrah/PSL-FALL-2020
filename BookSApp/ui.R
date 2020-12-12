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
    dashboardHeader(title = "Book Recommender"),
    dashboardSidebar(disable = TRUE),
    
    dashboardBody(
      fluidRow(
        box(width = 12, title = "Step 1: Rate as many books as possible", 
            status = "info", solidHeader = TRUE, collapsible = TRUE,
            uiOutput('ratings')),
      ),#fluidRow 1
      
      fluidRow(
        box(title = "Histogram box title",
            status = "warning", solidHeader = TRUE, collapsible = TRUE,
            plotOutput("distPlot", height = 250)
        )
      )#fluidRow 2
    )#dashboardBody
  )#dashpoardPage
)#shinyUI
