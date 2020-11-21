library(text2vec)
library(glmnet)
library(tm)

set.seed(1234)
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
# it_word_select = itoken(train$review,
#                   preprocessor = tolower,
#                   tokenizer = word_tokenizer)
# tmp.vocab = create_vocabulary(it_word_select,
#                               stopwords = stop_words,
#                               ngram = c(1L,4L))
# tmp.vocab = prune_vocabulary(tmp.vocab, term_count_min = 10,
#                              doc_proportion_max = 0.5,
#                              doc_proportion_min = 0.001)
# dtm_word_select  = create_dtm(it_word_select, vocab_vectorizer(tmp.vocab))
ngram_vectorizer = vocab_vectorizer(create_vocabulary(myvocab, 
                                                ngram = c(1L, 2L)))

# Load training
train = read.table("train.tsv", stringsAsFactors = FALSE, header = TRUE)
train$review = gsub('<.*?>', ' ', train$review)
it_train = itoken(train$review,
                  preprocessor = tolower,
                  tokenizer = word_tokenizer)
dtm_train = create_dtm(it_train, ngram_vectorizer)

#Load test
test = read.table("test.tsv", stringsAsFactors = FALSE, header = TRUE)
test$review = gsub('<.*?>', ' ', test$review)
it_test = itoken(test$review,
                  preprocessor = tolower,
                  tokenizer = word_tokenizer)
dtm_test = create_dtm(it_test, ngram_vectorizer)

# Train classification model
class_mdl = glmnet(
  x = dtm_train,
  y = train$sentiment,
  alpha = 0,
  family = 'binomial'
  )

# Compute and save predictions
mypred = predict(class_mdl, dtm_test, type = "response")
output = data.frame(id = test$id, prob = as.vector(mypred))
write.table(output,
            file = "mysubmission.txt",
            row.names = FALSE,
            sep = '\t', 
            append = F)
