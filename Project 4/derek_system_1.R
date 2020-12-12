#'system 1' genre based recommendations
library(recommenderlab)
library(Matrix)
library(dplyr)
library(plyr)
library(data.table)

# get the data ----
##ratings
myurl = "https://liangfgithub.github.io/MovieData/"
ratings = read.csv(paste0(myurl, 'ratings.dat?raw=true'), 
                   sep = ':',
                   colClasses = c('integer', 'NULL'), 
                   header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')
ratings = ratings[, -4]   #dont need timestamp

##movies
movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)  #seperated by ::
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)  #is a list of lists
movies = data.frame(movies, stringsAsFactors = FALSE)  # one line per movie unlike the ratings
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)  #treats MovieID as a character which makes difficult to matchup

# convert accented characters
movies$Title[73]
movies$Title = iconv(movies$Title, "latin1", "UTF-8")
movies$Title[73]

# extract year from movie title
movies$Year = as.numeric(unlist(
  lapply(movies$Title, function(x) substr(x, nchar(x)-4, nchar(x)-1))))  

##load genre recommendations
genre_recs = as.data.frame(read.csv('genre_recommendations.csv', header = T))

#system 1derek - genre recommendation ----
create_margin_ratings()
write_recommendations()

##create margin and raw_margin rating ----
create_margin_ratings = function() {
  n_movies = nrow(movies)
  movies$margin_rating = NA
  movies$raw_margin = NA
  
  for (i in 1:n_movies) {
    if (i %% 500 == 0) {
      print(paste0(i, " out of ", n_movies))
    }
    
    #snag each movie by their ID#
    id = movies$MovieID[i]
    
    #get all ratings matching that ID#
    ratings_for_movie = ratings[ratings$MovieID == id, "Rating"]
    ratings_for_movie = ratings_for_movie - 3 # center onto 0
    
    #get raw margin for exploration
    movies[i, "raw_margin"] = sum(ratings_for_movie)
    
    # calculate and store margin (sum + abs)
    movies[i, "margin_rating"] = abs(sum(ratings_for_movie))
  }
}

##write genre recommendations to file ----
write_recommendations = function() {
  genre_list = c("Action", "Adventure", "Animation",
    "Children's", "Comedy", "Crime", "Documentary",
    "Drama", "Fantasy", "Film-Noir", "Horror",
    "Musical", "Mystery", "Romance", "Sci-Fi",
    "Thriller", "War", "Western")
  
  #turn movies/genres into a boolean matrix of movies x genres
  genres = as.data.frame(movies$Genres, stringsAsFactors = FALSE)
  tmp = as.data.frame(tstrsplit(genres[, 1], '[|]',
                                type.convert = TRUE),
                      stringsAsFactors = FALSE)
  genre_matrix = matrix(0, nrow(movies), length(genre_list))
  for (i in 1:nrow(tmp)) {
    genre_matrix[i, genre_list %in% tmp[i, ]] = 1
  }
  colnames(genre_matrix) = genre_list
  remove("tmp", "genres")
  
  #create a matrix 50 movies x genre
  genre_recs_mat = matrix("-", 50, length(genre_list))
  colnames(genre_recs_mat) = genre_list
  
  #for each genre
  for (g in genre_list) {
    #grab all movies that are tagged with that genre
    m = which(genre_matrix[, g] == 1)
    movs = movies[m, c("Title", "margin_rating")]
    
    #sort by margin rating
    movs = arrange(movs, desc(margin_rating))
    genre_recs_mat[, g] = movs$Title[1:50]
  }
  
  #save recommendations for later
  write.csv(genre_recs_mat,
            file = "genre_recommendations.csv",
            row.names = F)
  
  #just to test
  # readitbacktome = as.data.frame(read.csv('genre_recommendations.csv', header = T))
}


##function to actually return genre recommendations ----
system1derek = function(sel_genre, n = 10){
  return(genre_recs[1:n, sel_genre])
}

#splitting genres
#most number of genres assigned to a movie?
#how many unique genres?
unique_combo_genres = unique(movies$Genres)
num_genres = integer(length = length(unique_combo_genres))
all_genres = list()

for(i in 1:length(unique_combo_genres)){
  # chars = unlist(strsplit(unique_combo_genres[i],""))  
  # num_genres[i] = sum(chars == "|")
  all_genres = c(all_genres, unlist(strsplit(unique_combo_genres[i], split = "|", fixed = T)))
}

clrs = c(rep('tomato', 8), rep('dodgerblue', 10))
hist(movies$raw_margin[movies$raw_margin < 800],
     xlim = c(-800, 800),
     breaks = 20,
     col = clrs,
     main = 'Raw Ratings within 2 Standard Deviations')

# system 2a ----
set.seed(0721)

#split training test data for system 2
train_idx = sample(nrow(ratings), floor(nrow(ratings)) * 0.8)
train = ratings[train_idx, ]
test = ratings[-train_idx, ]
head(train)
head(test)

##helper functions----
split_by_user = function(ratings, s = 0.8){
  unique_users = unique(ratings$UserID)
  train_users = sample(unique_users, length(unique_users) * s)
  
  train_ratings = ratings[ratings$UserID %in% train_users, ]
  test_ratings = ratings[!(ratings$UserID %in% train_users), ]
  return(list(train = train_ratings, test = test_ratings))
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
  
  #realRatingMatrix is an actual datatype.  For whatever reason Recommender() requires a specific type of object
  Rmat = new('realRatingMatrix', data = Rmat)
}

#for reduced testing run !!!!!!!!dont use in real run
sample_UserIDs = sample(ratings$UserID, size = 1000)
smol_ratings = ratings[ratings$UserID %in% sample_UserIDs, ]


#complete cycle for system 2 ----
num_iterations = 10
rmse = integer(num_iterations)

for(i in 1:num_iterations){
  print(paste0("On iteration #", i))
  
  Rmat  = create_rating_matrix(smol_ratings)
  r_eval_scheme = evaluationScheme(Rmat, 
                                   method = 'split', train = 0.8, 
                                   given = 10, goodRating = 5)
  rec_UBCF = Recommender(getData(r_eval_scheme, 'train'), 
                         method = 'SVD',
                         parameter = list(normalize = 'center', 
                                          k = 25))
  preds = predict(rec_UBCF, getData(r_eval_scheme, 'known'), type = 'ratings')
  
  #calculate RMSE
  error = calcPredictionAccuracy(preds, getData(r_eval_scheme, 'unknown'))
  rmse[i] = error[[1]]
}

rmse
print(paste0("Average RMSE: ", mean(rmse)))

##use Recommender labs built in functions for creating/predicting/evaluating ----
Rmat = create_rating_matrix(ratings)

normal_Rmat = normalize(Rmat)
getRatingMatrix(Rmat)[1:10, 1:10]
getRatingMatrix(normal_Rmat)[1:10, 1:10]
image(normal_Rmat[1:10, 1:20]) #produces a heatmap style plot
hist(getRatings(Rmat))
hist(getRatings(normal_Rmat))

r_eval_scheme_split = evaluationScheme(Rmat, 
                                 method = 'split', train = 0.8, 
                                  given = 10, goodRating = 5)
algos = list(
  "Random"  = list(name = "RANDOM", param = NULL),
  # 'Popular' = list(name = 'POPULAR', param = NULL),
  # 'UBCF'    = list(name = 'UBCF', param = list(nn = 25)),
  'IBCF'    = list(name = 'IBCF', param = list(k = 25))
  # 'SVD'     = list(name = 'SVD', param = list(k = 25))
  # 'SVD50'     = list(name = 'SVD', param = list(k = 50))
)

results_split = evaluate(r_eval_scheme, algos, type = 'ratings')

clrs = c('gray30', 'dodgerblue', 'orange', 'magenta4', 'tomato')
plot(results, 'prec/rec', 
     main = ,
     col = clrs)
title(main = "Scores using Full Dataset, 5-Fold CV, Given-10")
#using type = "topNList" gives wacky TPR/FPR evaluation since its only looking at a True Positive Rate involving N possible condididates (for example top 5)
# it will return a bananas amoutn of True Negatives since everything that isnt in the top N will not get returned

#explore IBCF evaluation wackyness
algos = list(
  "Random"  = list(name = "RANDOM", param = NULL),
  'IBCF'    = list(name = 'IBCF', param = list(k = 25))
  )
r_eval_scheme_split = evaluationScheme(Rmat, 
                                       method = 'split', train = 0.8, 
                                       given = 10, goodRating = 5)
results_split = evaluate(r_eval_scheme_split, algos, type = 'ratings')


r_eval_scheme_cv5 = evaluationScheme(Rmat, 
                                       method = 'cross', k = 5,
                                       given = 10, goodRating = 5)
results_cv5 = evaluate(r_eval_scheme_cv5, algos, type = 'ratings')

plot(results_split)
plot(results_cv5)


# test graph before full run
rmse_UBCF

bp = barplot(rmse_UBCF, xpd=F,
        ylim = c(0.6, 1.4),
        ylab = 'RMSE',
        xlab = 'Iteration',
        main = 'Error over 10 iterations for UBCF Recommender',
        col = 'orange',
        xaxt = NULL)
axis(1, at = c(1:10), labels = c(1:10))
text(x = c(1:10), y = rmse_UBCF + 0.4, labels = rmse_UBCF, cex=1) 
abline(h = mean(rmse_UBCF), lty = 4, lwd = 2)
