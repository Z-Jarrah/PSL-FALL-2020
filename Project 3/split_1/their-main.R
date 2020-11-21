library(tidyverse)
library(glmnet)
library(tm)
library(text2vec)
library(pROC)

setwd("~/Google Drive/Geek2/UofIll/CS598_PSL/Github/PSL-FALL-2020/Project 3")

# Creating the Test IDs ====================================================
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

# Load vocab and training data ----

source(mymain.R)

# run classifier on each split ----
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


# scoring ----
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