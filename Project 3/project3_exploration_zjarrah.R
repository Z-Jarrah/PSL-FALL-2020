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
myvocab <- scan(file = "myvocab.txt", what = character())
train <- read.table("train.tsv", stringsAsFactors = FALSE,
                    header = TRUE)

#####################################
#
# Train a binary classification model
#
#####################################


test <- read.table("test.tsv", stringsAsFactors = FALSE,
                   header = TRUE)

#####################################
# Compute prediction 
# Store your prediction for test data in a data frame
# "output": col 1 is test$id
#           col 2 is the predited probabilities
#####################################

write.table(output, file = "mysubmission.txt", 
            row.names = FALSE, sep='\t')

# ==============================================================================
# ==============================================================================
# ==============================================================================


# ==============================================================================
# ==============================================================================
# ==============================================================================
library(pROC)
# source(mymain.R)
# move test_y.tsv to this directory
test.y <- read.table("test_y.tsv", header = TRUE)
pred <- read.table("mysubmission.txt", header = TRUE)
pred <- merge(pred, test.y, by="id")
roc_obj <- roc(pred$sentiment, pred$prob)
pROC::auc(roc_obj)
