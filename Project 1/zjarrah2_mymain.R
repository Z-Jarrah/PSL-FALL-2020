#import and setup train/test data for a specifc train/test set
library(caret)
library(randomForest)
library(xgboost)
results.mdl1 = double(10)
results.mdl2 = double(10)

train = read.csv("train.csv", stringsAsFactors = F)
test  = read.csv("test.csv", stringsAsFactors = F)
true_price = read.csv("test_y.csv")

#model 1 - lasso chosen predictors to elastic net
#log transform the sale price then seperate the train data into predictor ~ response
train[, 83] = log(train[, 83])
y = as.numeric(unlist(train[83]))
X = train[, -c(1, 83)]

winsor.vars <- c("Lot_Frontage", "Lot_Area", "Mas_Vnr_Area", "BsmtFin_SF_2", "Bsmt_Unf_SF", "Total_Bsmt_SF", "Second_Flr_SF", 'First_Flr_SF', "Gr_Liv_Area", "Garage_Area", "Wood_Deck_SF", "Open_Porch_SF", "Enclosed_Porch", "Three_season_porch", "Screen_Porch", "Misc_Val")
quan.value <- 0.95
for(var in winsor.vars){
  tmp <- X[, var]
  myquan <- quantile(tmp, probs = quan.value, na.rm = TRUE)
  tmp[tmp > myquan] <- myquan
  X[, var] <- tmp
  
  tmp <- test[, var]
  myquan <- quantile(tmp, probs = quan.value, na.rm = TRUE)
  tmp[tmp > myquan] <- myquan
  test[, var] <- tmp
}

#process training data
categorical.vars <- colnames(X)[
  which(sapply(X, function(x) mode(x)=="character"))]
train.matrix <- X[, !colnames(X) %in% categorical.vars, drop=FALSE]
saved_levels = list()
n.train <- nrow(train.matrix)
for(var in categorical.vars){
  mylevels <- sort(unique(X[, var]))
  saved_levels[[var]] = mylevels
  m <- length(mylevels)
  m <- ifelse(m>2, m, 1)
  tmp.train <- matrix(0, n.train, m)
  col.names <- NULL
  for(j in 1:m){
    tmp.train[X[, var]==mylevels[j], j] <- 1
    col.names <- c(col.names, paste(var, '_', mylevels[j], sep=''))
  }
  colnames(tmp.train) <- col.names
  train.matrix <- cbind(train.matrix, tmp.train)
}

#process test data
test.matrix <- test[, !colnames(test) %in% categorical.vars, drop=FALSE]
n.test <- nrow(test.matrix)
for(var in categorical.vars){
  mylevels = saved_levels[[var]]
  m = length(mylevels)
  m <- ifelse(m>2, m, 1)
  temp_test <- matrix(0, n.test, m)
  col.names <- NULL
  for(j in 1:m){
    temp_test[test[, var]==mylevels[j], j] <- 1
    col.names <- c(col.names, paste(var, '_', mylevels[j], sep=''))
  }
  colnames(temp_test) <- col.names
  test.matrix <- cbind(test.matrix, temp_test)
}

set.seed(9618)
xgb.model <- xgboost(data = as.matrix(train.matrix), 
                     label = y, max_depth = 6,
                     eta = 0.05, nrounds = 5000,
                     subsample = 0.5,
                     verbose = FALSE)

test.matrix$PID = NULL
test_mat = data.matrix(test.matrix)
xgb_pred = predict(xgb.model, newdata=test_mat)

sqrt(mean((log(true_price$Sale_Price) - xgb_pred)^2))



#shift right due to PID in test matrix?
pred_xgb = exp(xgb_pred)

#write submission file
submission1 = cbind(PID = test$PID, Sale_Price = pred_xgb)
colnames(submission1) = c("PID", "Sale_Price")
write.csv(submission1, file = "mysubmission2.txt", row.names = F)