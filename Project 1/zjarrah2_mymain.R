library(glmnet)
library(randomForest)
library(caret)
train = read.csv("train.csv", stringsAsFactors = F)
test  = read.csv("test.csv", stringsAsFactors = F)
true_price = read.csv("test_y.csv")

#model 1 - lasso chosen predictors to elastic net
#log transform the sale price then separate the train data into predictor ~ response
train[, 83] = log(train[, 83])
y = as.numeric(unlist(train[83]))
X = train[, -c(1, 83)]
# stash PIDs for later use
# pids = test$PID
# test$PID = NULL

#remove a bunch of random or repeated features, remove Garage YR Blt due to missing values
X = subset(X, select = -c(Garage_Yr_Blt, Street, Utilities,  Condition_2, Roof_Matl, 
                          Heating, Pool_QC, Misc_Feature, Low_Qual_Fin_SF,
                          Pool_Area, Longitude, Latitude))

train2 = subset(train, select = -c(Garage_Yr_Blt, Street, Utilities,  Condition_2, Roof_Matl, 
                          Heating, Pool_QC, Misc_Feature, Low_Qual_Fin_SF,
                          Pool_Area, Longitude, Latitude))

test = subset(test, select = -c(Garage_Yr_Blt, Street, Utilities,  Condition_2, Roof_Matl, 
                                Heating, Pool_QC, Misc_Feature, Low_Qual_Fin_SF,
                                Pool_Area, Longitude,Latitude))

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

"Sale_Price" %in% names(train2) 
"Sale_Price" %in% names(test) 
str(train)

#RandomForest: Manual
rf_fit1 = randomForest(Sale_Price~., train2, ntree=100000)
rf_pred1 = predict(rf_fit1, test)
sqrt(mean((log(true_price$Sale_Price) - rf_pred1)^2))


#Using caret library for randomForest
control = trainControl(method='repeatedcv',
                        number=10,
                        repeats=3)
mtry = sqrt(ncol(train2))
tunegrid = expand.grid(.mtry=mtry)
rf_fit2 = train(Sale_Price~., data=train2, method='rf', metric='RMSE',
      tuneGrid=tunegrid, trControl=control)

rf_pred2 = predict(rf_fit2, newdata=head(test))
sqrt(mean((log(true_price$Sale_Price) - rf_pred2)^2))


#Using caret library for boosting
fitControl = trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

gbm1 = train(Sale_Price ~ ., data = train2, 
                 method = "gbm", 
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)

gbm_pred1 = predict(gbm1, newdata=test)

sqrt(mean((log(true_price$Sale_Price) - gbm_pred1)^2))




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

#shift right due to PID in test matrix?
test_select = data.matrix(test.matrix[, (sel_vars+1)])
pred_rf = predict(rf_fit1, test)

# write submission file
submission1 = cbind(PID = test$PID, Sale_Price = pred_rf)
colnames(submission1) = c("PID", "Sale_Price")
write.csv(submission1, file = "mysubmission1_zj.txt", row.names = F)
