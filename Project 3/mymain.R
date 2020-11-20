myvocab <- scan(file = "myvocab.txt", what = character())

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



set.seed(1234)
print(paste('starting on split ', j))

# Set working dir
split_dir = paste("split_", j, sep = "")
setwd(split_dir)

# Load training
train <-
  read.table("train.tsv", stringsAsFactors = FALSE, header = TRUE)
train$review = gsub('<.*?>', ' ', train$review)

#Load test
test <-
  read.table("test.tsv", stringsAsFactors = FALSE, header = TRUE)
test$review = gsub('<.*?>', ' ', test$review)

it_train = itoken(train$review,
                  preprocessor = tolower,
                  tokenizer = word_tokenizer)
dtm_train = create_dtm(it_train, my_vectorizer)

#####################################
# Train a binary classification model
set.seed(1234)
mylogit.cv = cv.glmnet(
  x = dtm_train,
  y = train$sentiment,
  alpha = 0,
  family = 'binomial',
  type.measure = "auc"
)

mylogit.fit = glmnet(
  x = dtm_train,
  y = train$sentiment,
  alpha = 0,
  lambda = mylogit.cv$lambda.min,
  family = 'binomial'
)

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
write.table(output,
            file = "mysubmission.txt",
            row.names = FALSE,
            sep = '\t')
