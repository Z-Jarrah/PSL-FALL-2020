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
movies$Title = iconv(movies$Title, "latin1", "UTF-8")

# extract year from movie title
movies$Year = as.numeric(unlist(
  lapply(movies$Title, function(x) substr(x, nchar(x)-4, nchar(x)-1))))  

##load genre recommendations
genre_recs = as.data.frame(read.csv('zj_genre_recommendations.csv', header = T))

#system 1

##create margin and raw_margin rating ----
create_popularity_rating = function() {
  n_movies = nrow(movies)
  movies$popularity_rating = NA
  
  for (i in 1:n_movies) {
    if (i %% 500 == 0) {
      print(paste0(i, " out of ", n_movies))
    }
    
    #snag each movie by their ID#
    id = movies$MovieID[i]
    
    #get all ratings matching that ID#
    ratings_for_movie = ratings[ratings$MovieID == id, "Rating"]
    
    if(length(ratings_for_movie) >= 100){
      avg_rating = mean(ratings_for_movie) # center onto 0
    }
    else{
      avg_rating = 0
    }
    # calculate and store margin (sum + abs)
    movies[i, "popularity_rating"] = avg_rating
  }
  
  return(movies)
}

movies = create_popularity_rating()

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
    movs = movies[m, c("MovieID", "popularity_rating")]
    
    #sort by margin rating
    movs = arrange(movs, desc(popularity_rating))
    genre_recs_mat[, g] = movs$MovieID[1:50]
  }
  
  #save recommendations for later
  write.csv(genre_recs_mat,
            file = "zj_genre_recommendations.csv",
            row.names = F)
  
}


##function to actually return genre recommendations ----
system1_part2 = function(sel_genre, n = 10){
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

write_recommendations()
system1_part2("Adventure")
