#system2_functions.R

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
  i = paste0('u', ratings_df$UserID) #user number ...
  j = paste0('m', ratings_df$MovieID) #movie number ...
  x = ratings_df$Rating
  
  #nessecary to prevent sparseMatrix freaking out over i + j being characters instead of integers
  tmp = data.frame(i, j, x, stringsAsFactors = T)
  Rmat = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
  #the levels for each are the order in which the data is already entered into the matrix
  rownames(Rmat) = levels(tmp$i)
  colnames(Rmat) = levels(tmp$j)
  
  #realRatingMatrix is an actual datatype.  
  #For whatever reason Recommender() requires a specific type of matrix object
  Rmat = new('realRatingMatrix', data = Rmat)
  return(Rmat)
}


##########DELETE
# code to make up some dummy user input
input = data.frame(movieID = sample(x = unique(movies$MovieID), size = 5, replace = F),
               Rating  = sample(c(1:5), size = 5, replace = T))


##reload ratings data