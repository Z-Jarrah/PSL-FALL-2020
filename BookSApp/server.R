#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/

library(shiny)
library(ShinyRatingInput)
library(data.table)
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

# load rating data ----
# ratings = read.csv(paste0(data_url, 'ratings.dat?raw=true'), 
                   # sep = ':',
                   # colClasses = c('integer', 'NULL'), 
                   # header = FALSE)
ratings = readLines("data/ratings.dat")
ratings = strsplit(ratings, split = "::", fixed = TRUE, useBytes = TRUE)  #seperated by ::
ratings = matrix(unlist(ratings), ncol = 4, byrow = TRUE)  #is a list of lists
ratings = data.frame(ratings, stringsAsFactors = FALSE)  # one line per movie unlike the ratings
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')
ratings = ratings[, -4]   #dont need timestamp


# process input value junk
get_user_ratings <- function(value_list) {
  dat <- data.table(movie_id = sapply(strsplit(names(value_list), "_"), function(x) ifelse(length(x) > 1, x[[2]], NA)),
                    rating = unlist(as.character(value_list)))
  dat <- dat[!is.null(rating) & !is.na(movie_id)]
  dat[rating == " ", rating := 0]
  dat[, ':=' (movie_id = as.numeric(movie_id), rating = as.numeric(rating))]
  dat <- dat[rating > 0]
  cat(file=stderr(), "reactvalues ", as.character(dat[1,1]))
  # get the indices of the ratings
  # add the user ratings to the existing rating matrix
  return(dat)
  # user_ratings <- sparseMatrix(i = dat$book_id, 
  #                              j = rep(1,nrow(dat)), 
  #                              x = dat$rating, 
  #                              dims = c(nrow(ratingmat), 1))
}

# server logic ----
shinyServer(function(input, output) {
  #get active user ratings
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
                  div(style = "text-align:center; color: black; font-size: 18px", movies$Title[mIDs_for_rating[i+j]]),
                  div(style = "text-align:center; font-size: 100%; color: gold;", ratingInput(paste0("select_", movies$MovieID[mIDs_for_rating[i+j]]), label = "", dataStop = 5))  #
              ))#/box/list inner
        })))#lapply/fluidRow/list outer
      })#/lapply
    )#fluidRow 1
  })#output$ratings
  
  retrieve_ratings <- eventReactive(input$btn, {
    # get the user's rating data
    value_list <- reactiveValuesToList(input)
    user_ratings <- get_user_ratings(value_list)
    # cat(file=stderr(), "reactvalues list", str(value_list))
    # 
    # # add user's ratings as first column to rating matrix
    # rmat <- cbind(user_ratings, ratingmat)
    # 
    # # get the indices of which cells in the matrix should be predicted
    # # predict all books the current user has not yet rated
    # items_to_predict <- which(rmat[, 1] == 0)
    # prediction_indices <- as.matrix(expand.grid(items_to_predict, 1))
    # 
    # # run the ubcf-alogrithm
    # res <- predict_cf(rmat, prediction_indices, "ubcf", TRUE, cal_cos, 1000, FALSE, 2000, 1000)
    # 
    # # sort, organize, and return the results
    # user_results <- sort(res[, 1], decreasing = TRUE)[1:20]
    # user_predicted_ids <- as.numeric(names(user_results))
    # recom_results <- data.table(Rank = 1:20, 
    #                             Book_id = user_predicted_ids, 
    #                             Author = books$authors[user_predicted_ids], 
    #                             Title = books$title[user_predicted_ids], 
    #                             Predicted_rating =  user_results)
    return(user_ratings)
  }) # clicked on button
  
  output$results <- renderUI({
    num_rows    = 4
    num_movies  = 5
    rec_results = retrieve_ratings()
    h3(rec_results)
    # lapply(1:num_rows, function(i) {
    #   list(fluidRow(lapply(1:num_books, function(j) {
    #     box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_books + j),
    #         
    #         div(style = "text-align:center", 
    #             a(href = paste0('https://www.goodreads.com/book/show/', books$best_book_id[recom_result$Book_id[(i - 1) * num_books + j]]), 
    #               target='blank', 
    #               img(src = books$image_url[recom_result$Book_id[(i - 1) * num_books + j]], height = 150))
    #         ),
    #         div(style = "text-align:center; color: #999999; font-size: 80%", 
    #             books$authors[recom_result$Book_id[(i - 1) * num_books + j]]
    #         ),
    #         div(style="text-align:center; font-size: 100%", 
    #             strong(books$title[recom_result$Book_id[(i - 1) * num_books + j]])
    #         )
    #         
    #     )        
      # }))) # columns
    # }) # rows
    
  }) # renderUI function
})#shinyServer
