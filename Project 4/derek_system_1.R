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
ratings = ratings[, -4]

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