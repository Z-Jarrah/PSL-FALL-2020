library(tidyverse)
library(glmnet)
library(tm)
library(text2vec)
library(pROC)

setwd("~/Google Drive/Geek2/UofIll/CS598_PSL/Github/PSL-FALL-2020/Project 3")

# ==============================================================================
# === Creating the Test IDs ====================================================
# ==============================================================================
data <- read.table("alldata.tsv", stringsAsFactors = FALSE,
                   header = TRUE)

testIDs <- read.csv("splits_F20.csv", header = TRUE)
for(j in 1:5){
  dir.create(paste("split_", j, sep=""))
  train <- data[-testIDs[,j], c("id", "sentiment", "review") ]
  test <- data[testIDs[,j], c("id", "review")]
  test.y <- data[testIDs[,j], c("id", "sentiment", "score")]
  
  tmp_file_name <- paste("split_", j, "/", "train.tsv", sep="")
  write.table(train, file=tmp_file_name, 
              quote=TRUE, 
              row.names = FALSE,
              sep='\t')
  tmp_file_name <- paste("split_", j, "/", "test.tsv", sep="")
  write.table(test, file=tmp_file_name, 
              quote=TRUE, 
              row.names = FALSE,
              sep='\t')
  tmp_file_name <- paste("split_", j, "/", "test_y.tsv", sep="")
  write.table(test.y, file=tmp_file_name, 
              quote=TRUE, 
              row.names = FALSE,
              sep='\t')
}
# ==============================================================================
# ==============================================================================
# ==============================================================================


# ==============================================================================
# === Binary Classification ====================================================
# ==============================================================================

#####################################
# Load your vocabulary and training data
#####################################

# Delete
# myvocab <- scan(file = "myvocab.txt", what = character())
# What we've tried (I)
# Construct DocumentTerm matrix (max 4-grams)
stop_words = c("i", "me", "my", "myself", 
               "we", "our", "ours", "ourselves", 
               "you", "your", "yours", 
               "their", "they", "his", "her", 
               "she", "he", "a", "an", "and",
               "is", "was", "are", "were", 
               "him", "himself", "has", "have", 
               "it", "its", "the", "us")
it_word_select = itoken(train$review,
                  preprocessor = tolower, 
                  tokenizer = word_tokenizer)
tmp.vocab = create_vocabulary(it_word_select,
                              stopwords = stop_words,
                              ngram = c(1L,4L))
tmp.vocab = prune_vocabulary(tmp.vocab, term_count_min = 10,
                             doc_proportion_max = 0.5,
                             doc_proportion_min = 0.001)
dtm_word_select  = create_dtm(it_word_select, vocab_vectorizer(tmp.vocab))

#select words
tmpfit_cv = cv.glmnet(x = dtm_word_select, 
                y = train$sentiment, 
                alpha = 1,
                family='binomial')
plot(tmpfit_cv)
fit_cv = glmnet(x = dtm_word_select, y = train$sentiment, 
                alpha = 1,
                lambda = tmpfit_cv$lambda.1se,
                family='binomial')

select_fit = glmnet(x = dtm_word_select, y = train$sentiment, 
                    alpha = 1,
                    family='binomial')

colnames(dtm_word_select)[which(abs(select_fit$beta[, 36]) > .75)]

myvocab = colnames(dtm_word_select)[which(select_fit$beta[, 12] != 0)]
my_vectorizer = vocab_vectorizer(create_vocabulary(myvocab, 
                                                ngram = c(1L, 2L)))


for(j in 1:5){
  set.seed(1234)
  print(paste('starting on split ', j))
  
  # Set working dir
  split_dir = paste("split_", j, sep="")
  setwd(split_dir)
  
  # Load training
  train <- read.table("train.tsv", stringsAsFactors = FALSE, header = TRUE)
  train$review = gsub('<.*?>', ' ', train$review)
  
  #Load test
  test <- read.table("test.tsv", stringsAsFactors = FALSE, header = TRUE)
  test$review = gsub('<.*?>', ' ', test$review)

  it_train = itoken(train$review,
                    preprocessor = tolower, 
                    tokenizer = word_tokenizer)
  dtm_train = create_dtm(it_train, my_vectorizer)
  
  #####################################
  # Train a binary classification model
  set.seed(1234)
  mylogit.cv = cv.glmnet(x = dtm_train, 
                         y = train$sentiment, 
                         alpha = 0,
                         family='binomial', 
                         type.measure = "auc")
  
  mylogit.fit = glmnet(x = dtm_train, 
                       y = train$sentiment, 
                       alpha = 0,
                       lambda = mylogit.cv$lambda.min, 
                       family='binomial')
  
  #####################################
  # Compute prediction 
  # Store your prediction for test data in a data frame
  # "output": col 1 is test$id
  #           col 2 is the predited probabilities
  #####################################
  it_test = itoken(test$review,
                   preprocessor = tolower, 
                   tokenizer = word_tokenizer)
  
  dtm_test = create_dtm(it_test, my_vectorizer)
  mypred = predict(mylogit.fit, dtm_test, type = "response")
  output = data.frame(id = test$id, prob = as.vector(mypred))
  write.table(output, file = "mysubmission.txt", 
              row.names = FALSE, sep='\t')
  
  # !!!Do not include in mymain.R!!!
  setwd('..')
}

# ==============================================================================

# source(mymain.R)
auc_scores = rep(0, 5)
for(i in 1:5){
  # Set working dir
  split_dir = paste0("split_", i)
  setwd(split_dir)
  
  test.y <- read.table("test_y.tsv", header = TRUE)
  pred <- read.table("mysubmission.txt", header = TRUE)
  pred <- merge(pred, test.y, by="id")
  roc_obj <- roc(pred$sentiment, pred$prob)
  auc_scores[i] = pROC::auc(roc_obj)
  setwd("..")
}

print(auc_scores)


# investigate length of reviews ----
it_word_select = itoken(data$review,
                        preprocessor = tolower, 
                        tokenizer = word_tokenizer)
full.vocab = create_vocabulary(it_word_select)
dtm_full_vocab  = create_dtm(it_word_select, vocab_vectorizer(full.vocab))
dim(dtm_full_vocab)

review_length = rowSums(dtm_full_vocab)
range(review_length)
data[which.max(review_length), ]
hist(review_length[review_length < 1000])

sum(review_length <700) / length(review_length)
short_idx = which(review_length < 700)

# random questions ----
table(data$sentiment)

# train a model only only review that are less than 700 words long
train_short = data[short_idx, ]
train_short$review = gsub('<.*?>', ' ', train_short$review)
stop_words = c("i", "me", "my", "myself", 
               "we", "our", "ours", "ourselves", 
               "you", "your", "yours", 
               "their", "they", "his", "her", 
               "she", "he", "a", "an", "and",
               "is", "was", "are", "were", 
               "him", "himself", "has", "have", 
               "it", "its", "the", "us")
it_word_select = itoken(train_short$review,
                        preprocessor = tolower, 
                        tokenizer = word_tokenizer)
short.vocab = create_vocabulary(it_word_select,
                              stopwords = stop_words,
                              ngram = c(1L,4L))
short.vocab = prune_vocabulary(short.vocab, term_count_min = 10,
                             doc_proportion_max = 0.5,
                             doc_proportion_min = 0.001)
dtm_word_short = create_dtm(it_word_select, vocab_vectorizer(short.vocab))
short_mdl = glmnet(x = dtm_word_select, y = train_short$sentiment, 
                  alpha = 1,
                  family='binomial')
which(short_mdl$df < 1000)

# topwords from model
colnames(dtm_word_select)[which(abs(short_mdl$beta[, 25]) > .5)]

myvocab = colnames(dtm_word_select)[which(short_mdl$beta[, 36] != 0)]
my_vectorizer = vocab_vectorizer(create_vocabulary(myvocab, 
                                                   ngram = c(1L, 2L)))
