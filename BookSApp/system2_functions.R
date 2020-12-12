#system2_functions.R


#assumes ratings and movies have already been loaded into a dataframe
#assumes new user ratings are being returned in a two column data frame of (movieID, Rating)
newuserID = 9999

#include the active user in the ratings dataframe
system2_recs = function(input, n = 10){
  active_user_ratings = cbind(newuserID, input)
  colnames(active_user_ratings) = c('UserID', 'MovieID', 'Rating')
  ratings = rbind(ratings, active_user_ratings)
  # 
  Rmat = create_rating_matrix(ratings)
  active_user_ID = paste0('u', newuserID)
  active_user_Rmat_row = which(rownames(Rmat) == active_user_ID)

  rec_SVD = Recommender(Rmat,
                        method = 'SVD',
                        parameter = list(normalize = 'center', k = 25, maxiter = 50))
  preds = predict(rec_SVD, Rmat[active_user_Rmat_row,], n = n)

  preds_list = as(preds, "list")[[1]]
  movie_ids = unlist(lapply(preds_list, left))
  cat(file=stderr(), "predicted movie ids", str(movie_ids))
  return(movie_ids)
}

#returns a movie ID without the m in front of the actual ID
left = function(string){
  return(substr(string, 2, nchar(string)))
}

create_rating_matrix = function(ratings_df){
  cat(file=stderr(), "creating sparsematrix")
  
  #create a sparse matrix with data x at location i,j
  u = paste0('u', ratings_df$UserID) #user number ...
  m = paste0('m', ratings_df$MovieID) #movie number ...
  x = ratings_df$Rating

  #nessecary to prevent sparseMatrix freaking out over i + j being characters instead of integers
  tmp = data.frame(u, m, x, stringsAsFactors = T)
  cat(file=stderr(), "creating sparsematrix")
  Rmat = sparseMatrix(as.integer(tmp$u), as.integer(tmp$m), x = tmp$x)
  #the levels for each are the order in which the data is already entered into the matrix
  rownames(Rmat) = levels(tmp$u)
  colnames(Rmat) = levels(tmp$m)
  
  #realRatingMatrix is an actual datatype.  
  #For whatever reason Recommender() requires a specific type of matrix object
  Rmat = new('realRatingMatrix', data = Rmat)
  return(Rmat)
}