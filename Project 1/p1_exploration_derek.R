nacols <- function(df) {
  colnames(df)[unlist(lapply(df, function(x) anyNA(x)))]
}

extra_lvls = function(train_c, test_c){
  extra_c = vector()
  for(c in test_c){
    if(!(c %in% train_c)){
      extra_c = append(extra_c, c)
    }
  }
  return(extra_c)
}

drop_columns = function(df, drop_cs){
  
}

#import and setup train/test data for a specifc train/test set
results.mdl1 = double(10)
results.mdl2 = double(10)

j = 2

for(j in 1:10) {
  testIDs = read.table("project1_testIDs.dat")
  data    = read.csv("Ames_data.csv")
  train_j = data[-testIDs[, j],]
  test_j  = data[testIDs[, j],-83]
  test_jy = data[testIDs[, j], c(1, 83)]
  
  #insert function to test model 1 here
  results.mdl1[j] = mdl1_fun(train_j, test_j, test_jy)
  results.mdl2[j] = mdl2_fun(train_j, test_j, test_jy)
}

mdl1_fun = function(train, test, test_y){
  #remove garage built year
  
}

mdl2_fun = function(train, test, test_y){
  
}

# write the test files for later pipeline testing into features and response
write.csv(train_j, file = "train.csv", row.names = F)
write.csv(test_j,  file = "test.csv", row.names = F)
write.csv(test_jy, file = "test_y.csv", row.names = F)

#ease of use
train = train_j
test = test_j
test_y = test_jy

###############  code for inclusion in mymain.R  ################################

#read in the files
train = read.csv("train.csv")
test  = read.csv("test.csv")
true_price = read.csv("test_y.csv")

#remove garage year built
train$Garage_Yr_Blt = NULL
test$Garage_Yr_Blt = NULL

#convert price to log for prediction
train[, 82] = log(train[, 82])

#model 1
library(glmnet)
y = as.numeric(unlist(train[82]))
X = train[, -c(1, 82)]

#try with all dummy columns first
Xd = dummy_cols(X, remove_first_dummy = T, remove_selected_columns = T)
Xdm = as.matrix(Xd)
thelasso = glmnet(Xdm, y, alpha = 1)
cv_thelasso = cv.glmnet(Xdm, y, alpha = 1)
plot(cv_thelasso)
xdm_lasso = glmnet(Xdm, y, alpha = 1)
plot(xdm_lasso, label = TRUE, xvar = "lambda")

#try without a subset of predictors
Xr = subset(X, select = -c(Street, Utilities,  Condition_2, Roof_Matl, 
                                    Heating, Pool_QC, Misc_Feature, Low_Qual_Fin_SF,
                                    Pool_Area, Longitude,Latitude))


#try 1
#subset of only high value non-categorical predictors
{
  Xr = subset(X, select = c(Kitchen_AbvGr, Fireplaces, Garage_Cars, Year_Built)) #Overall_Qual, Sale_Type, Bldg_Type, Bsmt_Exposure, Exter_Qual
Xm = as.matrix(Xr)
cv.out = cv.glmnet(Xm, y, alpha = 1)

# sel.vars = predict(cv.out, type="nonzero", 
#                     s = cv.out$lambda.1se)$X1
# cv.out <- cv.glmnet(as.matrix(train.matrix[, sel.vars]), 
#                     train.y, alpha = 0)
test_r = subset(test, select = c(Kitchen_AbvGr, Fireplaces, Garage_Cars, Year_Built))
tmp = predict(cv.out, s = cv.out$lambda.min, 
              newx = data.matrix(test_r))
}


#try 2
#all high value predictors turned into dummy variables
{
Xrc = subset(X, select = c(Kitchen_AbvGr, Fireplaces, Garage_Cars, Year_Built,
                           Overall_Qual, Sale_Type, Bldg_Type, Bsmt_Exposure, Exter_Qual))
Xd = dummy_cols(Xrc, remove_first_dummy = T, remove_selected_columns = T)
Xdm = data.matrix(Xd)
cv.xdm = cv.glmnet(Xdm, y, alpha = 1)

test_rc = subset(test, select = c(Kitchen_AbvGr, Fireplaces, Garage_Cars, Year_Built,
                                Overall_Qual, Sale_Type, Bldg_Type, Bsmt_Exposure, Exter_Qual))
levels(test_rc$Overall_Qual) = levels(train$Overall_Qual)
levels(test_rc$Sale_Type) = levels(train$Sale_Type)
test_xdm = dummy_cols(test_rc, remove_first_dummy = T, remove_selected_columns = T)
test_xdm = data.matrix(test_xdm)

pred_xdm = predict(cv.xdm, s = cv.xdm$lambda.min, 
                   newx = data.matrix(test_xdm))
}

#try3
#same as above but scaled
{
Xrc = subset(X, select = c(Kitchen_AbvGr, Fireplaces, Garage_Cars, Year_Built,
                           Overall_Qual, Sale_Type, Bldg_Type, Bsmt_Exposure, Exter_Qual))
Xd = dummy_cols(Xrc, remove_first_dummy = T, remove_selected_columns = T)
Xdm = data.matrix(Xd)
scaled_X = apply(Xdm, 2, scale)

cv_scaled = cv.glmnet(Xdm, y, alpha = 1, standardize = T)

test_rc = subset(test, select = c(Kitchen_AbvGr, Fireplaces, Garage_Cars, Year_Built,
                                  Overall_Qual, Sale_Type, Bldg_Type, Bsmt_Exposure, Exter_Qual))
levels(test_rc$Overall_Qual) = levels(train$Overall_Qual)
levels(test_rc$Sale_Type) = levels(train$Sale_Type)
test_xdm = dummy_cols(test_rc, remove_first_dummy = T, remove_selected_columns = T)
test_xdm = data.matrix(test_xdm)
scaled_test = apply(test_xdm, 2, scale)

pred_scaled = predict(cv_scaled, s = cv_scaled$lambda.1se, 
                   newx = data.matrix(test_xdm))
}

#try 4
#use her code to select the best variables using lasso including dummy variables then use ridge for final model
Xr = subset(X, select = -c(Street, Utilities,  Condition_2, Roof_Matl, 
                           Heating, Pool_QC, Misc_Feature, Low_Qual_Fin_SF,
                           Pool_Area, Longitude,Latitude))
Xrd = dummy_cols(Xrc, remove_first_dummy = T, remove_selected_columns = T)
Xr_matrix = data.matrix(Xrd)
cv_select = cv.glmnet(Xr_matrix, y, alpha = 1)
sel_vars = predict(cv_select, type="nonzero", s = cv_select$lambda.1se)$X1
ridge_select = cv.glmnet(Xr_matrix[, sel_vars], y, alpha = 0)

###need to setup test data same way
test_r = subset(test, select = -c(Street, Utilities,  Condition_2, Roof_Matl, 
                           Heating, Pool_QC, Misc_Feature, Low_Qual_Fin_SF,
                           Pool_Area, Longitude,Latitude))
test_d = dummy_cols(test_r, remove_first_dummy = T, remove_selected_columns = T)
test_dm = data.matrix(test_d)

pred_select = predict(ridge_select, s = ridge_select$lambda.1se, 
                      newx = (test_dm[, sel_vars]))


#try5
#websites version using model.matrix
{
y = as.numeric(unlist(train[82]))
X = train[, -c(1, 82)]

X_5 = X
X_5$train = 1
test_copy = test[, -1]
test_copy$train = 0
full = rbind(X_5, test_copy)

full_dummies = dummy_cols(full, remove_first_dummy = T, remove_selected_columns = T)
train_X = full_dummies[full_dummies$train == 1, ]
test_X = full_dummies[full_dummies$train == 0, ]

# full_model = model.matrix(Sale_Price ~ ., full)[, -1]
# train_X = full_model[full_model[,'train'] == 1, ]
# test_X = full_model[full_model[,'train'] == 0, ]

train_X = data.matrix(train_X)
test_X = data.matrix(test_X)
ames_lasso = glmnet(x = train_X, y, alpha = 0)
plot(ames_lasso, xvar = "lambda")

ames_lasso_cv = cv.glmnet(x = train_X, y, alpha = 1)
plot(ames_lasso_cv)

pred_lasso = predict(ames_lasso_cv, s = ames_lasso_cv$lambda.1se, test_X)
sqrt(mean((abs(pred_lasso) - log(true_price$Sale_Price))^2))
}

#try6
#without outliers and those ones she says to remove
{
library(glmnet)
train = read.csv("train.csv")
test  = read.csv("test.csv")
true_price = read.csv("test_y.csv")

#remove garage year built
train$Garage_Yr_Blt = NULL
test$Garage_Yr_Blt = NULL

train = train[train$Sale_Price < 400000, ]
train = train[train$Sale_Price > 75000, ]

idx_remove = true_price$Sale_Price < 400000
test = test[idx_remove, ]
# test = test[true_price$Sale_Price > 75000, ]
true_price = true_price[idx_remove, ]
# true_price = true_price[true_price$Sale_Price > 75000, ]
#convert price to log for prediction
train[, 82] = log(train[, 82])

y = as.numeric(unlist(train[82]))
X = train[, -c(1, 82)]

Xr = subset(X, select = -c(Street, Utilities,  Condition_2, Roof_Matl, 
                           Heating, Pool_QC, Misc_Feature, Low_Qual_Fin_SF,
                           Pool_Area, Longitude,Latitude))
test = subset(test, select = -c(Street, Utilities,  Condition_2, Roof_Matl, 
                                  Heating, Pool_QC, Misc_Feature, Low_Qual_Fin_SF,
                                  Pool_Area, Longitude,Latitude))

boxplot(train$Sale_Price)

X_5 = Xr
X_5$train = 1
test_copy = test[, -1]
test_copy$train = 0
full = rbind(X_5, test_copy)

full_dummies = dummy_cols(full, remove_first_dummy = T, remove_selected_columns = T)
train_X = full_dummies[full_dummies$train == 1, ]
test_X = full_dummies[full_dummies$train == 0, ]

# full_model = model.matrix(Sale_Price ~ ., full)[, -1]
# train_X = full_model[full_model[,'train'] == 1, ]
# test_X = full_model[full_model[,'train'] == 0, ]

train_X = data.matrix(train_X)
test_X = data.matrix(test_X)

ames_lasso_cv = cv.glmnet(x = train_X, y, alpha = 0)
pred_lasso = predict(ames_lasso_cv, s = ames_lasso_cv$lambda.1se, test_X)
sqrt(mean((abs(pred_lasso) - log(true_price$Sale_Price))^2))
}

#try 7
# hers with the winsorization thing
{
library(glmnet)
train = read.csv("train.csv", stringsAsFactors = T)
test  = read.csv("test.csv", stringsAsFactors = T)
true_price = read.csv("test_y.csv")

train[, 83] = log(train[, 83])
y = as.numeric(unlist(train[83]))
X = train[, -c(1, 83)]
test$PID = NULL

# train$Garage_Yr_Blt = NULL
# test$Garage_Yr_Blt = NULL

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

X_7 = X
X_7$train = 1
test_copy = test
test_copy$train = 0
full = rbind(X_7, test_copy)

full_dummies = dummy_cols(full, remove_first_dummy = T, remove_selected_columns = T)
X_dummies = full_dummies[full_dummies$train == 1, ]
test_dummies = full_dummies[full_dummies$train == 0, ]

# X_dummies = dummy_cols(X, remove_first_dummy = T, remove_selected_columns = T)
# test_dummies = dummy_cols(test, remove_first_dummy = T, remove_selected_columns = T)

X_dummies = data.matrix(X_dummies)
test_dummies = data.matrix(test_dummies)

cv_select = cv.glmnet(X_dummies, y, alpha = 1)
sel_vars = predict(cv_select, type="nonzero", s = cv_select$lambda.1se)$X1
ridge_select = cv.glmnet(X_dummies[, sel_vars], y, alpha = 0)


pred_select = predict(ridge_select, s = ridge_select$lambda.min, 
                      newx = (test_dummies[, sel_vars]))
sqrt(mean((abs(pred_select) - log(true_price$Sale_Price))^2))
}

#try 8
#her version but without the train/test 'cheat'
{
library(glmnet)
library(fastDummies)
train = read.csv("train.csv", stringsAsFactors = T)
test  = read.csv("test.csv", stringsAsFactors = T)
true_price = read.csv("test_y.csv")

#log transform the sale price then seperate the train data into predictor ~ response
train[, 83] = log(train[, 83])
y = as.numeric(unlist(train[83]))
X = train[, -c(1, 83)]

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

X_dummies = dummy_cols(X, remove_first_dummy = T, remove_selected_columns = T)
test_dummies = dummy_cols(test, remove_first_dummy = T, remove_selected_columns = T)

X_dummies = data.matrix(X_dummies)
test_dummies = data.matrix(test_dummies)

cv_select = cv.glmnet(X_dummies, y, alpha = 1)
sel_vars = predict(cv_select, type="nonzero", s = cv_select$lambda.1se)$X1
ridge_select = cv.glmnet(X_dummies[, sel_vars], y, alpha = 0)

lasso_mdl = glmnet(X_dummies, y, alpha = 1, lambda = cv_select$lambda.1se)

extra_l = extra_lvls(colnames(X_dummies), colnames(test_dummies))
copy = test_dummies[,!(colnames(test_dummies) %in% extra_l)]
copy = data.matrix(copy)
# test_dummies = subset(test_dummies, select = -c(missing_cols))

pred_lasso = predict(lasso_mdl, newx = copy)

pred_select = predict(ridge_select, s = ridge_select$lambda.min, 
                      newx = (copy[, sel_vars]))
sqrt(mean((abs(pred_select) - log(true_price$Sale_Price))^2))
}



#get levels
name = colnames(train)
diff_levels = vector()
i = 1
for(n in 1:ncol(test)){
  if(all.equal(levels(X[, n]), levels(test[, n])) != T){
    diff_levels[i] = colnames(test)[n]
    i = i + 1
  }
}


#try0
#refit with optimum lambda value
{
lr = glmnet(Xdm, y, alpha = 1, lambda = 410)

mdl_1 = lm(Sale_Price ~ ., data = train)
summary(mdl_1)

#glmnet doesnt like factor variables
include = vector()
c = 1
for(i in 1:ncol(X)){
  if(is.factor(X[, i]) == FALSE){
    # X_no_factors = cbind(X_no_factors,train[, i])
    include[c] = colnames(X[i])
    c = c+1
  }
}

Xm_no_factors = as.matrix(subset(X, select = include))
lasso_no_factors = glmnet(Xm_no_factors, y, alpha = 1)
plot(lasso_no_factors, label = TRUE, xvar = "lambda")

Xa = as.matrix(Xa)
cv_lasso_no_factors = cv.glmnet(Xa, y, alpha = 1)

min_lam_lasso = glmnet(Xm_no_factors, y, alpha = 1, lambda = 200)
}


#model 2


#test the models

#need to convert back the price for submission