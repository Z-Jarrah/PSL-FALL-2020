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
  print(j)
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
  print(j)
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
  # stop_words = c("i", "me", "my", "myself", 
  #                "we", "our", "ours", "ourselves", 
  #                "you", "your", "yours", 
  #                "their", "they", "his", "her", 
  #                "she", "he", "a", "an", "and",
  #                "is", "was", "are", "were", 
  #                "him", "himself", "has", "have", 
  #                "it", "its", "the", "us")
  
  #Use the tm package to have more comprehensive stop words
  stop_words = stopwords(kind = "en")
  
  stemmed_train_reviews = tm::stemDocument(train$review)
  it_train = itoken(stemmed_train_reviews,
                    preprocessor = tolower, 
                    tokenizer = word_tokenizer)
  
  # What we've tried (I)
  tmp.vocab = create_vocabulary(it_train,
                                stopwords = stop_words,
                                ngram = c(1L,4L))
  tmp.vocab = prune_vocabulary(tmp.vocab, term_count_min = 25,
                               doc_proportion_max = 0.5,
                               doc_proportion_min = 0.005)
  dtm_train  = create_dtm(it_train, vocab_vectorizer(tmp.vocab))
  
  set.seed(1234)
  tmpfit = glmnet(x = dtm_train, 
                  y = train$sentiment, 
                  alpha = 1,
                  family='binomial')
  # tmpfit$df[78]
  
  
  best_df = length(tmpfit$df)
  for(i in length(tmpfit$df):1){
    if(tmpfit$df[i] <= 1001){
      best_df = i
      print("Best df is:")
      print(i)
      break
    }
  }
  myvocab = colnames(dtm_train)[which(tmpfit$beta[, best_df] != 0)]
  
  print("Vocab size is:")
  print(length(myvocab))
  
  
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
  
  
  stemmed_test_reviews = tm::stemDocument(test$review)
  it_test = itoken(stemmed_test_reviews,
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
for(j in 1:5){
  print(j)
  split_dir = paste("split_", j, sep="")
  setwd(split_dir)
  test.y <- read.table("test_y.tsv", header = TRUE)
  pred <- read.table("mysubmission.txt", header = TRUE)
  pred <- merge(pred, test.y, by="id")
  roc_obj <- roc(pred$sentiment, pred$prob)
  print(pROC::auc(roc_obj))
  setwd("..")
}
