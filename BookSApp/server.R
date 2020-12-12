#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
data_url = "https://liangfgithub.github.io/MovieData/"

#load movie data ----
movies = readLines(paste0(data_url, 'movies.dat?raw=true'))
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)  #seperated by ::
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)  #is a list of lists
movies = data.frame(movies, stringsAsFactors = FALSE)  # one line per movie unlike the ratings
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)  #treats MovieID as a character which makes difficult to matchup
movies$Title = iconv(movies$Title, "latin1", "UTF-8") # convert accented characters

# extract year from movie title
movies$Year = as.numeric(unlist(
  lapply(movies$Title, function(x) substr(x, nchar(x)-4, nchar(x)-1))))  

# load rating data ----
ratings = read.csv(paste0(data_url, 'ratings.dat?raw=true'), 
                   sep = ':',
                   colClasses = c('integer', 'NULL'), 
                   header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')
ratings = ratings[, -4]   #dont need timestamp

# server logic ----
shinyServer(function(input, output) {
  output$ratings <- renderUI({
    num_rows   = 3
    num_movies = 5 #columns
    
    #get movies to show for rating - random
    mIDs_for_rating = sample(movies$MovieID, num_rows * num_movies)
    
    
    fluidRow(
      lapply(1:num_rows, function(i) {
        list(fluidRow(lapply(1:num_movies, function(j) {
          list(box(width = 2,
                  # div(style = "text-align:center", img(src = 'https://i.gr-assets.com/images/S/compressed.photo.goodreads.com/books/1566239899l/49960031.jpg', height = 150)),
                  div(style = "text-align:center; color: black; font-size: 18px", "Funny Bunny"),
                  div(style = "text-align:center; color: #999999; font-size: 80%", movies$Title[mIDs_for_rating[i]])
                  # div(style = "text-align:center", strong(books$title[(i - 1) * num_books + j])),
                  # div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", movies$book_id[(i - 1) * num_books + j]), label = "", dataStop = 5))))
              ))#/box/list inner
        })))#lapply/fluidRow/list outer
      })#/lapply
    )#fluidRow 1
  })#output$ratings
  
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = 20 + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
  })#output$distPlot
  
})
