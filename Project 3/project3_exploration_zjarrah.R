library(tidyverse)
library(glmnet)
library(tm)
library(text2vec)
library(pROC)

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

for(j in 1:5){
  split_dir = paste("split_", j, sep="")
  # Set working dir
  setwd(split_dir)
  # Load training
  split_dir = paste("train.tsv", sep="")
  train <- read.table(split_dir, stringsAsFactors = FALSE,
                      header = TRUE)
  train$review = gsub('<.*?>', ' ', train$review)
  #Load test
  split_dir = paste("test.tsv", sep="")
  test <- read.table(split_dir, stringsAsFactors = FALSE,
                      header = TRUE)
  test$review = gsub('<.*?>', ' ', test$review)
  
  # Construct DocumentTerm matrix (max 4-grams)
  stop_words = c("i", "me", "my", "myself", 
                 "we", "our", "ours", "ourselves", 
                 "you", "your", "yours", 
                 "their", "they", "his", "her", 
                 "she", "he", "a", "an", "and",
                 "is", "was", "are", "were", 
                 "him", "himself", "has", "have", 
                 "it", "its", "the", "us")
  it_train = itoken(train$review,
                    preprocessor = tolower, 
                    tokenizer = word_tokenizer)
  
  # What we've tried (I)
  tmp.vocab = create_vocabulary(it_train,
                                stopwords = stop_words,
                                ngram = c(1L,4L))
  tmp.vocab = prune_vocabulary(tmp.vocab, term_count_min = 10,
                               doc_proportion_max = 0.5,
                               doc_proportion_min = 0.001)
  dtm_train  = create_dtm(it_train, vocab_vectorizer(tmp.vocab))
  
  set.seed(1234)
  tmpfit = glmnet(x = dtm_train, 
                  y = train$sentiment, 
                  alpha = 1,
                  family='binomial')
  tmpfit$df[78]
  
  myvocab = colnames(dtm_train)[which(tmpfit$beta[, 78] != 0)]
  
  
  vectorizer = vocab_vectorizer(create_vocabulary(myvocab, 
                                                  ngram = c(1L, 2L)))
  dtm_train = create_dtm(it_train, vectorizer)
  
  #####################################
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
  #####################################
  
  
  
  it_test = itoken(test$review,
                   preprocessor = tolower, 
                   tokenizer = word_tokenizer)
  
  dtm_test = create_dtm(it_test, vectorizer)
  mypred = predict(mylogit.fit, dtm_test, type = "response")
  output = data.frame(id = test$id, prob = as.vector(mypred))
  
  #####################################
  # Compute prediction 
  # Store your prediction for test data in a data frame
  # "output": col 1 is test$id
  #           col 2 is the predited probabilities
  #####################################
  
  write.table(output, file = "mysubmission.txt", 
              row.names = FALSE, sep='\t')
  
  # !!!Do not include in mymain.R!!!
  setwd('..')
}
# ==============================================================================
# ==============================================================================
# ==============================================================================


# ==============================================================================
# ==============================================================================
# ==============================================================================

# source(mymain.R)
# move test_y.tsv to this directory
setwd("split_5")
test.y <- read.table("test_y.tsv", header = TRUE)
pred <- read.table("mysubmission.txt", header = TRUE)
pred <- merge(pred, test.y, by="id")
roc_obj <- roc(pred$sentiment, pred$prob)
pROC::auc(roc_obj)
setwd("..")
