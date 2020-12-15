#system2_functions.R
library(dplyr)
library(data.table)
library(Matrix)
library(plyr)
library(recommenderlab)

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

sample_UserIDs = sample(ratings$UserID, size = 1000)
ratings = ratings[ratings$UserID %in% sample_UserIDs, ]

#assumes ratings and movies have already been loaded into a dataframe
#assumes new user ratings are being returned in a two column data frame of (movieID, Rating)
newuserID = 9999

#include the active user in the ratings dataframe
system2_recs = function(input, n = 10){
  active_user_ratings = cbind(newuserID, input)
  colnames(active_user_ratings) = c('UserID', 'MovieID', 'Rating')
  ratings = rbind(ratings, active_user_ratings)

  Rmat = create_rating_matrix(ratings)
  active_user_ID = paste0('u', newuserID)
  active_user_Rmat_row = which(rownames(Rmat) == active_user_ID)

  rec_SVD = Recommender(Rmat,
                        method = 'SVD',
                        parameter = list(normalize = 'center', k = 25, maxiter = 50))
  preds = predict(rec_SVD, Rmat[active_user_Rmat_row,], n = n)

  preds_list = as(preds, "list")[[1]]
  movie_ids = unlist(lapply(preds_list, left))
  return(movie_ids)
}

#returns a movie ID without the m in front of the actual ID
left = function(string){
  return(substr(string, 2, nchar(string)))
}

create_rating_matrix = function(ratings_df){
  #create a sparse matrix with data x at location i,j
  u = paste0('u', ratings_df$UserID) #user number ...
  m = paste0('m', ratings_df$MovieID) #movie number ...
  x = ratings_df$Rating
  
  cat("in rating matrix")

  #nessecary to prevent sparseMatrix freaking out over i + j being characters instead of integers
  tmp = data.frame(u, m, x, stringsAsFactors = T)
  Rmat = sparseMatrix(as.integer(tmp$u), as.integer(tmp$m), x = as.integer(tmp$x))
  
  #the levels for each are the order in which the data is already entered into the matrix
  rownames(Rmat) = levels(tmp$u)
  colnames(Rmat) = levels(tmp$m)
  
  Rmat = new('realRatingMatrix', data = Rmat)
  return(Rmat)
}