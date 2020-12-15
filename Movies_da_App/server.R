# server.R

library(dplyr)
library(data.table)
library(Matrix)
library(plyr)
library(recommenderlab)
library(shiny)
library(ShinyRatingInput)

data_url = "https://liangfgithub.github.io/MovieData/"

#load movie data ----
# movies = readLines(paste0(data_url, 'movies.dat?raw=true'))
movies = readLines("data/movies.dat")
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)  #seperated by ::
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)  #is a list of lists
movies = data.frame(movies, stringsAsFactors = FALSE)  # one line per movie unlike the ratings
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)  #treats MovieID as a character which makes difficult to matchup
movies$Title = iconv(movies$Title, "latin1", "UTF-8") # convert accented characters

# extract year from movie title
movies$Year = as.numeric(unlist(
  lapply(movies$Title, function(x) substr(x, nchar(x)-4, nchar(x)-1))))  


# process input value into a sensible dataframe
get_user_ratings <- function(value_list) {
  dat <- data.table(movie_id = sapply(strsplit(names(value_list), "_"), function(x) ifelse(length(x) > 1, x[[2]], NA)),
                    rating = unlist(as.character(value_list)))
  dat <- dat[!is.null(rating) & !is.na(movie_id)]
  dat[rating == " ", rating := 0]
  dat[, ':=' (movie_id = as.numeric(movie_id), rating = as.numeric(rating))]
  dat <- dat[rating > 0]

  return(dat)
}

# server logic ----
shinyServer(function(input, output) {
  output$genre_selection <- renderUI({
    selected_genre = input$genre_select_menu
    if(selected_genre == ""){h3("Select a Genre to see some recommendations")}
    else{
      selected_genre = gsub("'", ".", selected_genre)
      selected_genre = gsub("-", ".", selected_genre)
      
      num_rows    = 1
      num_movies  = 5
      rec_results = as.integer(system1_recs(selected_genre)) #returns a simple list
      
      lapply(1:num_rows, function(i) {
        list(fluidRow(lapply(1:num_movies, function(j) {
          current_movieID = rec_results[(i-1)*num_movies + j]
          img_url = paste0("https://liangfgithub.github.io/MovieImages/", current_movieID, ".jpg?raw=true")
          
          box(width = 2, status = "success", solidHeader = FALSE, 
              #         div(style = "text-align:center",
              #             a(href = paste0('https://www.goodreads.com/book/show/', books$best_book_id[recom_result$Book_id[(i - 1) * num_books + j]]),
              #               target='blank',
              #               img(src = books$image_url[recom_result$Book_id[(i - 1) * num_books + j]], height = 150))
              #         ),
              div(style = "text-align:center", img(src = img_url, height = 150)),
              div(style = "text-align:center; color: darkred; font-size: 18px",
                  movies$Title[movies$MovieID == current_movieID])
              )#box
          }))) #list/fluidRow/lapply/function - columns
        }) #lapply/function - rows
    }#else
  })#genre_selection
  
  #display sample movies for active user to rate
  output$ratings <- renderUI({
    num_rows   = 3
    num_movies = 5 #columns
    
    #get movies to show for rating - random
    # mIDs_for_rating = sample(movies$MovieID, num_rows * num_movies)
    mIDs_for_rating = as.integer(names(sort(table(ratings$MovieID, useNA = "no"), decreasing = T)[1:50]))
    
    
    fluidRow(
      lapply(1:num_rows, function(i) {
        list(fluidRow(lapply(1:num_movies, function(j) {
          current_movieID = mIDs_for_rating[(i-1)*num_movies + j]
          img_url = paste0("https://liangfgithub.github.io/MovieImages/", current_movieID, ".jpg?raw=true")
          
          list(box(width = 2,
                  div(style = "text-align:center", img(src = img_url, height = 150)),
                  div(style = "text-align:center; color: black; font-size: 18px", 
                      movies$Title[movies$MovieID == current_movieID]),
                  div(style = "text-align:center; font-size: 100%; color: gold;", 
                      ratingInput(paste0("select_", current_movieID), label = "", dataStop = 5))  #
              ))#/box/list inner
        })))#lapply/fluidRow/list outer
      })#/lapply
    )#fluidRow 1
  })#output$ratings
  
  #get active users input after they click the button
  retrieve_ratings <- eventReactive(input$btn, {
    # get the user's rating data
    value_list <- reactiveValuesToList(input)
    user_ratings <- get_user_ratings(value_list)
    return(system2_recs(user_ratings))
  }) #retrieve_ratings - clicked on button
  
  output$results <- renderUI({
    num_rows    = 2
    num_movies  = 5
    rec_results = as.integer(retrieve_ratings()) #returns a simple list
  
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        current_movieID = rec_results[(i-1)*num_movies + j]
        img_url = paste0("https://liangfgithub.github.io/MovieImages/", current_movieID, ".jpg?raw=true")
        
        box(width = 2, status = "success", solidHeader = TRUE, 
            title = paste0("Rank ", ((i-1)*num_movies + j)),
    #         div(style = "text-align:center",
    #             a(href = paste0('https://www.goodreads.com/book/show/', books$best_book_id[recom_result$Book_id[(i - 1) * num_books + j]]),
    #               target='blank',
    #               img(src = books$image_url[recom_result$Book_id[(i - 1) * num_books + j]], height = 150))
    #         ),
            div(style = "text-align:center", img(src = img_url, height = 150)),
            div(style = "text-align:center; color: darkred; font-size: 18px",
                movies$Title[movies$MovieID == current_movieID])
    
        )#box
      }))) #list/fluidRow/lapply/function - columns
    }) #lapply/function - rows
  }) # output$results renderUI function
})#shinyServer
