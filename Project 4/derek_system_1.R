#'system 1' genre based recommendations
library(recommenderlab)
library(Matrix)

# get the data ----
myurl = "https://liangfgithub.github.io/MovieData/"
ratings = read.csv(paste0(myurl, 'ratings.dat?raw=true'), 
                   sep = ':',
                   colClasses = c('integer', 'NULL'), 
                   header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')
ratings = ratings[, -4]   #dont need timestamp

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

#system 1derek - genre recommendation ----

##create margin and raw_margin rating ----
n_movies = nrow(movies)
movies$margin_rating = NA
movies$raw_margin = NA

for(i in 1:n_movies){
  if(i %% 10 == 0){print(paste0(i, " out of ", n_movies))}
  
  #snag each movie by their ID#
  id = movies$MovieID[i]
  
  #get all ratings matching that ID#
  ratings_for_movie = ratings[ratings$MovieID == id, "Rating"]
  
  # center onto 0
  ratings_for_movie = ratings_for_movie - 3
  
  #get raw margin for exploration
  movies[i, "raw_margin"] = sum(ratings_for_movie)
  
  # calculate and store margin (sum + abs)
  movies[i, "margin_rating"] = abs(sum(ratings_for_movie))
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


# system 2a ----
#split training test data for system 2
set.seed(0721)
train_idx = sample(nrow(ratings), floor(nrow(ratings)) * 0.8)
train = ratings[train_idx, ]
test = ratings[-train_idx, ]
head(train)
head(test)

#create a sparse matrix with data x at location i,j
i = paste0('u', train$UserID) #user number ...
j = paste0('m', train$MovieID) #movie number ...
x = train$Rating
#nessecary to prevent sparseMatrix freaking out over i + j being characters instead of integers
tmp = data.frame(i, j, x, stringsAsFactors = T)
Rmat = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
#the levels for each are the order in which the data is already entered into the matrix
rownames(Rmat) = levels(tmp$i)
colnames(Rmat) = levels(tmp$j)

#realRatingMatrix is an actual datatype.  For whatever reason Recommender() requires a specific type of object
Rmat = new('realRatingMatrix', data = Rmat)

#creates a S4 recommender object that is a bit black-box ish
rec_UBCF = Recommender(Rmat, method = 'UBCF',
                       parameter = list(normalize = 'Z-score', 
                                        method = 'Cosine', 
                                        nn = 25))
rec_UBCF@model

#creates a different object that is also of type realRatingMatrix so you cant look at it directly and most convert it
recom = predict(rec_UBCF, 
                Rmat[1:3], type = 'ratings')
as(recom, 'matrix')[, 1:10]
