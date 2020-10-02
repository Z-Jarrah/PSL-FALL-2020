#import and setup train/test data for a specifc train/test set
results.mdl1 = double(10)
results.mdl2 = double(10)

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
  
}

mdl2_fun = function(train, test, test_y){
  
}

# write the test files for later pipeline testing into features and response
write.csv(train_j, file = "train.csv", row.names = F)
write.csv(test_j,  file = "test.csv", row.names = F)
write.csv(test_jy, file = "test_y.csv", row.names = F)

###############  code for inclusion in mymain.R  ################################
library(glmnet)

#read in the files
train = read.csv("train.csv")
test  = read.csv("test.csv")


#model 1
y = as.numeric(unlist(train[83]))
X = as.matrix(train[-c(1, 60, 83)])
X = as.data.frame(train[-c(1, 60, 83)])
thelasso = glmnet(X, y, alpha = 1)

mdl_1 = lm(Sale_Price ~ ., data = train)
summary(mdl_1)

#model 2


#test the models
true_price = read.csv("test_y.csv")
sqrt(mean((pred1 - true_price)^2))