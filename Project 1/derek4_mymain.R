train = read.csv("train.csv", stringsAsFactors = F)
test  = read.csv("test.csv", stringsAsFactors = F)
true_price = read.csv("test_y.csv")

#model 1 - lasso chosen predictors to elastic net
#log transform the sale price then seperate the train data into predictor ~ response
train[, 83] = log(train[, 83])
y = as.numeric(unlist(train[83]))
X = train[, -c(1, 83)]
# stash PIDs for later use
# pids = test$PID
# test$PID = NULL

#remove a bunch of random or repeated fatures, remove Garage YR Blt due to missing values
X = subset(X, select = -c(Garage_Yr_Blt, Street, Utilities,  Condition_2, Roof_Matl, 
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

#select variables and create model
t_matrix = data.matrix(train.matrix)
cv_select = cv.glmnet(t_matrix, y, alpha = 0.8)
sel_vars = predict(cv_select, type="nonzero", s = cv_select$lambda.1se)$X1
ridge_select = glmnet(t_matrix[, sel_vars], y, lambda = cv_select$lambda.1se, alpha = 0.2)

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
pred_ridge = predict(ridge_select, newx = test_select)

#write submission file
submission1 = cbind(PID = test$PID, Sale_Price = pred_ridge)
colnames(submission1) = c("PID", "Sale_Price")
write.csv(submission1, file = "mysubmission1.txt", row.names = F)
