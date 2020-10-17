library(glmnet)
library(xgboost)
library(randomForest)

#import and setup train/test data for a specifc train/test set
results.lasso   = double(10)
results.ridge   = double(10)
results.select  = double(10)
results.slr     = double(10)
results.xgb     = double(10)
results.rforest = double(10)

# for winsorization scores
qs = seq(0.5, 1, by = 0.05)
results.qs = double(length(qs))

for(j in 1:10) {
  print(cat("Iteration ", j, "   ##############"))
  testIDs = read.table("project1_testIDs.dat")
  data    = read.csv("Ames_data.csv", stringsAsFactors = F)
  train_j = data[-testIDs[, j],]
  test_j  = data[testIDs[, j],-83]
  test_jy = data[testIDs[, j], c(1, 83)]
  
  
  set.seed(0271)
  print("Lasso")
  results.lasso[j] = lasso_fun(train_j, test_j, test_jy)
  
  set.seed(0271)
  print("Ridge")
  results.ridge[j] = ridge_fun(train_j, test_j, test_jy)
  
  set.seed(0271)
  print("Elastic Net")
  results.select[j] = lasso_select_elasticnet_fun(train_j, test_j, test_jy)
  
  set.seed(0271)
  print("SLR")
  results.slr[j] = slr_fun(train_j, test_j, test_jy)
  
  set.seed(0271)
  print("XGB")
  results.xgb[j] = xgb_fun(train_j, test_j, test_jy, rounds = 500)
  
  set.seed(0271)
  print("Random Forest")
  results.rforest[j] = rand_forest_fun(train_j, test_j, test_jy, rounds = 500)
  
  #for winsorization scores
  # for(i in 1:length(qs)){
  #   print(i)
  #   set.seed(0271)
  #   results.qs[i] = results.qs[i] + mdl3_fun(train_j, test_j, test_jy, qs[i])
  # }
}

train = train_j
test = test_j
test_y = test_jy

#winzoriation vizualitzation
{
  results.qs = results.qs / 10
plot(results.qs,
     col = 'dodgerblue',
     pch = 19,
     cex = 1.25,
     xaxt = 'none',
     main = "Effect of Winsorization Quantile Cut Off",
     ylab = "Average RMSE",
     xlab = "Quantile Cut-Off")
axis(1, at = seq(1, length(qs)), labels = qs)
grid()
}

#average score vizualization
model_types = c("Selected Linear Regression", "Lasso", "Ridge", "Lasso to ElasticNet", 
                "Random Forest", "XGBoost")
scores = c(mean(results.slr), mean(results.lasso), mean(results.ridge), 
           mean(results.select), mean(results.rforest), mean(results.xgb))
colors = c('darkred','darkred','darkred',3,'darkred',3)
barplot(scores, ylim = c(0, 0.18),
        ylab = "Average RMSE over the 10 Splits",
        col = colors)
label_location = c(0.75, 1.9,3.1, 4.35, 5.5, 6.7)
axis(1, at = label_location, labels = model_types)
title(main = "Comparison of RMSE Scores Across Models", line=0)
abline(h = 0.125, lty = 'dashed', col = 'black')

drop_levels = function(df, mdl){
  lvls_names = names(mdl$xlevels)
  rows_nas = vector()
  for(n in lvls_names){
    # if the test observation has a differnt level for the predictor turn it into an NA
    for(i in 1:nrow(df)){
      if(!(df[i, n] %in% mdl$xlevels[[n]])){
        rows_nas = append(rows_nas, i)
        df[i,n] = NA
      }
    }
  }
  return(rows_nas)
}

#lasso only
lasso_fun = function(train, test, test_y, q = 0.95){
  #log transform the sale price then seperate the train data into predictor ~ response
  train[, 83] = log(train[, 83])
  y = as.numeric(unlist(train[83]))
  X = train[, -c(1, 83)]
  
  # stash PIDs for later use so columns arent off by one
  pids = test$PID
  test$PID = NULL
  
  #remove a bunch of random or repeated fatures, remove Garage YR Blt due to missing values
  X = subset(X, select = -c(Garage_Yr_Blt, Street, Utilities,  Condition_2, Roof_Matl, 
                            Heating, Pool_QC, Misc_Feature, Low_Qual_Fin_SF,
                            Pool_Area, Longitude, Latitude))
  test = subset(test, select = -c(Garage_Yr_Blt, Street, Utilities,  Condition_2, Roof_Matl, 
                                  Heating, Pool_QC, Misc_Feature, Low_Qual_Fin_SF,
                                  Pool_Area, Longitude,Latitude))
  
  winsor.vars <- c("Lot_Frontage", "Lot_Area", "Mas_Vnr_Area", "BsmtFin_SF_2", "Bsmt_Unf_SF", "Total_Bsmt_SF", "Second_Flr_SF", 'First_Flr_SF', "Gr_Liv_Area", "Garage_Area", "Wood_Deck_SF", "Open_Porch_SF", "Enclosed_Porch", "Three_season_porch", "Screen_Porch", "Misc_Val")
  quan.value <- q
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
  
  #create model
  t_matrix = data.matrix(train.matrix)
  cv_lambda = cv.glmnet(t_matrix, y, alpha = 1)
  the_lasso = glmnet(t_matrix, y, lambda = cv_lambda$lambda.1se, alpha = 1)
  
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
  test.matrix = data.matrix(test.matrix)
  pred = predict(the_lasso, newx = test.matrix)
  
  return(sqrt(mean((abs(pred) - log(test_y$Sale_Price))^2)))
}

#ridge only
ridge_fun = function(train, test, test_y, q = 0.95){
  #log transform the sale price then seperate the train data into predictor ~ response
  train[, 83] = log(train[, 83])
  y = as.numeric(unlist(train[83]))
  X = train[, -c(1, 83)]
  # stash PIDs for later use
  pids = test$PID
  test$PID = NULL
  
  #remove a bunch of random or repeated fatures, remove Garage YR Blt due to missing values
  X = subset(X, select = -c(Garage_Yr_Blt, Street, Utilities,  Condition_2, Roof_Matl, 
                            Heating, Pool_QC, Misc_Feature, Low_Qual_Fin_SF,
                            Pool_Area, Longitude, Latitude))
  test = subset(test, select = -c(Garage_Yr_Blt, Street, Utilities,  Condition_2, Roof_Matl, 
                                  Heating, Pool_QC, Misc_Feature, Low_Qual_Fin_SF,
                                  Pool_Area, Longitude,Latitude))
  
  winsor.vars <- c("Lot_Frontage", "Lot_Area", "Mas_Vnr_Area", "BsmtFin_SF_2", "Bsmt_Unf_SF", "Total_Bsmt_SF", "Second_Flr_SF", 'First_Flr_SF', "Gr_Liv_Area", "Garage_Area", "Wood_Deck_SF", "Open_Porch_SF", "Enclosed_Porch", "Three_season_porch", "Screen_Porch", "Misc_Val")
  quan.value <- q
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
  cv_lambda = cv.glmnet(t_matrix, y, alpha = 0)
  da_ridge = glmnet(t_matrix, y, lambda = cv_lambda$lambda.1se, alpha = 0)
  
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
  test_select = data.matrix(test.matrix)
  pred = predict(da_ridge, newx = test_select)
  
  return(sqrt(mean((abs(pred) - log(test_y$Sale_Price))^2)))
}

#lasso selection to elastic net
lasso_select_elasticnet_fun = function(train, test, test_y, q = 0.95){
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
  quan.value <- q
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
  elastic_select = glmnet(t_matrix[, sel_vars], y, lambda = cv_select$lambda.1se, alpha = 0.2)
  
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
  pred = predict(elastic_select, newx = test_select)
  
  return(sqrt(mean((abs(pred) - log(test_y$Sale_Price))^2)))
}

#slr
slr_fun = function(train, test, test_y){
  train[, 83] = log(train[, 83])
  y = as.numeric(unlist(train[83]))
  X = train[, -c(1, 82)]
  
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
  
  X = subset(X, select = c(Lot_Area, Year_Built, Year_Remod_Add, Total_Bsmt_SF, Garage_Cars, 
                           Gr_Liv_Area, Bsmt_Full_Bath, Full_Bath, Kitchen_AbvGr, Fireplaces,
                           Screen_Porch, Neighborhood, Overall_Qual,Bsmt_Qual, Central_Air, 
                           Kitchen_Qual, Functional, Sale_Condition, Overall_Cond))
  test_subset = subset(test, select = c(Lot_Area, Year_Built, Year_Remod_Add, Total_Bsmt_SF, Garage_Cars, 
                                        Gr_Liv_Area, Bsmt_Full_Bath, Full_Bath, Kitchen_AbvGr, Fireplaces,
                                        Screen_Porch, Neighborhood, Overall_Qual,Bsmt_Qual, Central_Air, 
                                        Kitchen_Qual, Functional, Sale_Condition, Overall_Cond))
  
  lm_mdl = lm(y ~ ., data = X)
  
  rows_w_nas = drop_levels(test_subset, lm_mdl)
  matched_true = test_y$Sale_Price
  if(length(rows_w_nas) > 0){
    test_subset = test_subset[-rows_w_nas, ]
    matched_true = test_y$Sale_Price[-rows_w_nas]
  }
  
  pred = predict(lm_mdl, newdata = test_subset)
  return(sqrt(mean((abs(pred) - log(matched_true))^2)))
}

#xgboost
xgb_fun = function(train, test, test_y, rounds = 500){
  train[, 83] = log(train[, 83])
  y = as.numeric(unlist(train[83]))
  X = train[, -c(1, 83)]
  
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
  
  xgb.model <- xgboost(data = as.matrix(train.matrix), 
                       label = y, max_depth = 6,
                       eta = 0.05, nrounds = rounds,
                       subsample = 0.5,
                       verbose = FALSE)
  
  test.matrix$PID = NULL
  test_mat = data.matrix(test.matrix)
  xgb_pred = predict(xgb.model, newdata=test_mat)
  
  return(sqrt(mean((log(test_y$Sale_Price) - xgb_pred)^2)))
}

#random forest
rand_forest_fun = function(train, test, test_y, rounds = 500){
  train[, 83] = log(train[, 83])
  train$Sale_Type[train$Sale_Type == "WD "] = "WD"
  y = as.numeric(unlist(train[83]))
  X = train[, -c(1, 83)]
  
  X = subset(X, select = -c(Garage_Yr_Blt, Street, Utilities,  Condition_2, Roof_Matl, 
                            Heating, Pool_QC, Misc_Feature, Low_Qual_Fin_SF,
                            Pool_Area, Exterior_1st, Exterior_2nd, Longitude, Latitude))
  test = subset(test, select = -c(Garage_Yr_Blt, Street, Utilities,  Condition_2, Roof_Matl, 
                                  Heating, Pool_QC, Misc_Feature, Low_Qual_Fin_SF,
                                  Pool_Area, Longitude,Latitude))
  
  winsor.vars <- c("Lot_Frontage", "Mas_Vnr_Area", "BsmtFin_SF_2", 
                   "Bsmt_Unf_SF", "Total_Bsmt_SF", "Second_Flr_SF", 'First_Flr_SF', 
                   "Gr_Liv_Area", "Garage_Area", "Wood_Deck_SF", "Open_Porch_SF", 
                   "Enclosed_Porch", "Three_season_porch", "Screen_Porch", "Misc_Val")
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
  
  fit = randomForest(y ~ ., data=train.matrix, ntree=100)
  rf_pred = predict(fit, test.matrix)
  return(sqrt(mean((log(test_y$Sale_Price) - rf_pred)^2)))
}
