#ui.R
#load rsconnect in console
#run rsconnect::deployApp()
library(shiny)
library(shinydashboard)

source('system1_functions.R')
source('system2_functions.R')

# Local image thing Add directory of static resources to Shiny's web server
duckit = "imgResources/duck_it.png"
addResourcePath(prefix = "imgResources", directoryPath = "images")
genre_list = c("", "Action", "Adventure", "Animation",
               "Children's", "Comedy", "Crime", "Documentary",
               "Drama", "Fantasy", "Film-Noir", "Horror",
               "Musical", "Mystery", "Romance", "Sci-Fi",
               "Thriller", "War", "Western")

# Define UI for application that draws a histogram
shinyUI(
  dashboardPage(
    skin = "blue",
    dashboardHeader(title = "Movies!"),
    dashboardSidebar(disable = TRUE),
    
    dashboardBody(
      fluidRow(
        box(width = 12, title = "Genre Suggestions", 
            status = "info", solidHeader = TRUE, collapsible = TRUE,
            selectInput(inputId = "genre_select_menu", 
                        label = "Select a Genre:",
                        choices = genre_list,
                        width = '250px',
                        selected = ""),
            tableOutput("genre_selection")
            # uiOutput('ratings')
            )#box
      ),#fluidRow 1 genre suggestions
      
      fluidRow(
        box(width = 12, title = "Personalized Recommendations - Step 1: Rate movies", 
            status = "info", solidHeader = TRUE, collapsible = TRUE,
            uiOutput('ratings'))
      ),#fluidRow 2 ratings
      
      fluidRow(
        # useShinyjs(),
        box(
          width = 12, status = "info", solidHeader = TRUE,
          title = "Personalized Recommendations - Step 2: Movies you might like",
          br(),
          actionButton("btn", "Click to get your highly scientific personalized recommendations", class = "btn-warning"),
          br(),
          tableOutput("results")
        )#box
      )#fluidRow 3 results
    )#dashboardBody
  )#dashpoardPage
)#shinyUI

