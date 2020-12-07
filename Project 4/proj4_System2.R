library(recommenderlab)
library(Matrix)

myurl = "https://liangfgithub.github.io/MovieData/"
ratings = read.csv(paste0(myurl, 'ratings.dat?raw=true'), 
                   sep = ':',
                   colClasses = c('integer', 'NULL'), 
                   header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')

movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)

# convert accented characters
movies$Title[73]
movies$Title = iconv(movies$Title, "latin1", "UTF-8")
movies$Title[73]

# extract year
movies$Year = as.numeric(unlist(
  lapply(movies$Title, function(x) substr(x, nchar(x)-4, nchar(x)-1))))

#==== Derek test code ==========================================================
sample_Mov_IDs = sample(movies$MovieID, size = 1000)
sample_movies = movies[sample_Mov_IDs, ]
sample_ratings = ratings[ratings$MovieID %in% sample_Mov_IDs, ]
#===============================================================================

set.seed(100)
# Change back when finished with testing
train.id = sample(nrow(sample_ratings), floor(nrow(sample_ratings)) * 0.8)
train = sample_ratings[train.id, ]
head(train)

test = sample_ratings[-train.id, ]
head(test)


i = paste0('u', train$UserID)
j = paste0('m', train$MovieID)
x = train$Rating
tmp = data.frame(i, j, x, stringsAsFactors = T)
Rmat = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
rownames(Rmat) = levels(tmp$i)
colnames(Rmat) = levels(tmp$j)
Rmat = new('realRatingMatrix', data = Rmat)


range(train$MovieID)
length(unique(train$MovieID))
range(train$UserID)
length(unique(train$UserID))
dim(Rmat)


rec_UBCF = Recommender(Rmat, method = 'UBCF',
                       parameter = list(normalize = 'Z-score', 
                                        method = 'Cosine', 
                                        nn = 25))
#Summary of model parameters
rec_UBCF@model


# predict ratings for the first three users
recom = predict(rec_UBCF, 
                Rmat[1:3], type = 'ratings')

as(recom, 'matrix')[, 1:10]

# This may take a long time
recom = predict(rec_UBCF, 
                Rmat, type = 'ratings')  
rec_list = as(recom, 'list')  # each element are ratings of that user

test.pred = test
test.pred$rating = NA

# For all lines in test file, one by one
for (u in 1:nrow(test)){
  
  # Read userid and movieid from columns 2 and 3 of test data
  userid = paste0("u", as.character(test$UserID[u]))
  movieid = paste0("m", as.character(test$MovieID[u]))
  
  rating = rec_list[[userid]][movieid]
  # handle missing values; 2.5 might not be the ideal choice
  test.pred$rating[u] = ifelse(is.numeric(rating), rating, 2.5)
}

# Calculate RMSE
sqrt(mean((test$rating - test.pred$rating)^2)) 

recommenderRegistry$get_entry_names()