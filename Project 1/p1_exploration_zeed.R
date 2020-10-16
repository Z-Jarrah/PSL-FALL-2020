#import and setup train/test data for a specifc train/test set
library(caret)
library(randomForest)
library(xgboost)

data = read.csv("Ames_Data.csv", stringsAsFactors = F)
testIDs = read.table("project1_testIDs.dat")
results.mdl1 = double(10)
for(j in 1:10) {
  train_j = data[-testIDs[, j],]
  test_j  = data[testIDs[, j],-83]
  test_jy = data[testIDs[, j], c(1, 83)]
  
  #insert function to test model 1 here
  results.mdl1[j] = mdl1_fun(train_j, test_j, test_jy)
}

mdl1_fun = function(train, test, test_y){
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
  
  set.seed(123)
  xgb.model <- xgboost(data = as.matrix(train.matrix), 
                       label = y, max_depth = 6,
                       eta = 0.05, nrounds = 5000,
                       subsample = 0.5,
                       verbose = FALSE)
  
  test.matrix$PID = NULL
  test_mat = data.matrix(test.matrix)
  xgb_pred = predict(xgb.model, newdata=test_mat)
  
  return(sqrt(mean((log(test_y$Sale_Price) - xgb_pred)^2)))
}












# ==============================================================================
# === Exploration section that may or may not be included ======================
# ==============================================================================

# # Need to preprocess AMES data prior to train/test
# preprocess =function(data){
#   data2=data
#   data2$Garage_Yr_Blt = NULL
#   
#   # subset(data, select = -c("Garage_Yr_Blt"))
# }
# 
# # write the test files for later pipeline testing into features and response
# write.csv(train_j, file = "train.csv", row.names = F)
# write.csv(test_j,  file = "test.csv", row.names = F)
# write.csv(test_jy, file = "test_y.csv", row.names = F)
# 
# ###############  code for inclusion in mymain.R  ################################
# library(glmnet)
# 
# #read in the files
# train = read.csv("train.csv")
# test  = read.csv("test.csv")
# 
# 
# #model 1
# y = as.numeric(unlist(train[83]))
# X = as.matrix(train[-c(1, 60, 83)])
# X = as.data.frame(train[-c(1, 60, 83)])
# thelasso = glmnet(X, y, alpha = 1)
# 
# mdl_1 = lm(Sale_Price ~ ., data = train)
# summary(mdl_1)
# 
# # === model 2 - randomForest ===================================================
# 
# set.seed(9618)
# oob_err = double(10)
# test_err = double(10)
# 
# proc_train = train
# proc_train$Garage_Yr_Blt = NULL
# 
# proc_test = test
# proc_test$Garage_Yr_Blt = NULL
# 
# fit = randomForest(Sale_Price~.,data=proc_train, ntree=100)
# rf_pred = predict(fit, proc_test)
# str(rf_pred)
# str(true_price$Sale_Price)
# sum(is.na(true_price))
# sum(is.na(rf_pred))
# 
# 
# # for(mtry in 1:10){
# # fit = randomForest(Sale_Price~., data=train, mtry=mtry, ntree=500)
# # oob_err[mtry] = fit$mse[500]
# # 
# # #pred = predict(fit, test)
# # 
# # #test_err[mtry] = with(test, mean((Sale_Price-pred)^2))
# # cat(mtry, " ")
# # }
# 
# # ==============================================================================
# 
# #test the models
# true_price = read.csv("test_y.csv")
# sqrt(mean((pred1 - true_price)^2))
# 
# 
# #test model (randomForest)
# sqrt(mean((log(rf_pred) - log(true_price$Sale_Price))^2))
