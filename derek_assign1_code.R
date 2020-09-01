##### assignment 1 test code
library(class)

#helper functions
generate_samples = function(s = 1, num_train = 200, num_test = 10000){
  num_centers = 10
  p = 2
  
  #distribution 'centers'
  m1 = matrix(rnorm(num_centers*p), num_centers, p) * s + cbind(rep(1, num_centers), rep(0, num_centers))
  m0 = matrix(rnorm(num_centers*p), num_centers, p) * s + cbind(rep(0, num_centers), rep(1, num_centers))
  
  
  # s = sqrt(1/5)
  #training data
  id1 = sample(1:num_centers, num_train, replace = T)
  id0 = sample(1:num_centers, num_train, replace = T)
  train_data = matrix(rnorm(2*num_train*p), 2*num_train, p)*s  + rbind(m1[id1, ], m0[id0, ])
  Ytrain = factor(c(rep(1, num_train), rep(0, num_train)))
  train = as.data.frame(cbind(Ytrain, train_data))
  
  #testing data
  id1 = sample(1:num_centers, num_test, replace = T)
  id0 = sample(1:num_centers, num_test, replace = T)
  test_data = matrix(rnorm(2*num_test*p), 2*num_test, p)*s + rbind(m1[id1, ], m0[id0, ])
  Ytest = factor(c(rep(1, num_test), rep(0, num_test)))
  test = as.data.frame(cbind(Ytest, test_data))
  samples = list(train = train, test = test)
  }

#get cross-validated error rate for a single k value
get_cv_k_err = function(data, Y, k_val, folds = 10){
  error = 0
  for(fold in 1:folds){
    #create our folds (split train/test data)
    testset_index = ((fold - 1)*fold_size + 1) : (fold*fold_size)
    trainX = data[-testset_index, ]
    trainY = Y[-testset_index]
    testX = data[testset_index, ]
    testY = Y[testset_index]
    
    predY = knn(trainX, testX, trainY, k = k_val)
    error = error + sum(testY != predY)
  }
  error = error/nrow(data)
}

#get the best k size for a given data/response set
get_best_k = function(data, Y, folds = 10){
  fold_size = floor(nrow(data)/folds)
  ks = seq(1, nrow(data) - fold_size, 2) 
  cv_k_err_rates = rep(0, length(ks))  
  
  #get error rates for all k values
  for(k_i in 1:length(ks)){
    cv_k_err_rates[k_i] = get_cv_k_err(data, Y, ks[k_i], folds)
  }
  
  #return best k value based on lowest error
  max(ks[cv_k_err_rates == min(cv_k_err_rates)])
}


#repeat process 20x
seed = 0721
set.seed(seed)
#**********fix********#
sims = 10
err = data.frame(
  slr.train  = rep(0, sims),
  slr.test   = rep(0, sims),
  quad.train = rep(0, sims),
  quad.test  = rep(0, sims),
  knn.train  = rep(0, sims),
  knn.test   = rep(0, sims)
  )

for(i in 1:sims){
  print(paste("on sim", i))
  samples = generate_samples()
  trainX = samples$train[c("V2", "V3")]
  trainY = samples$train$Ytrain
  testX  = samples$test[c("V2", "V3")]
  testY  = samples$test$Ytest
  
  #slr  #train data
  slr_mdl = lm(as.numeric(Ytrain) - 1 ~ ., data = samples$train)
  slr_train_pred   = as.numeric(slr_mdl$fitted.values > 0.5)
  err$slr.train[i] = sum(slr_train_pred != samples$train$Ytrain) / nrow(samples$train)
  
  #test data
  slr_test_pred   = ifelse(predict(slr_mdl, newdata = samples$test) > 0.5, 1, 0)
  err$slr.test[i] = sum(slr_test_pred != samples$test$Ytest) / nrow(samples$test)
  
  #quadratic   #train data
  quadratic_mdl     = lm((as.numeric(Ytrain) - 1) ~ V2 + V3 + I(V2*V3) + I(V2^2) + I(V3^2), data = samples$train)
  quad_train_pred   = as.numeric(quadratic_mdl$fitted.values > 0.5)
  err$quad.train[i] = sum(quad_train_pred != samples$test$Ytest) / nrow(samples$test)
  
  #quadratic test data
  quad_test_pred   = ifelse(predict(quadratic_mdl, newdata = samples$test) > 0.5, 1, 0)
  err$quad.test[i] = sum(quad_test_pred != samples$test$Ytest) / nrow(samples$test)
  
  #knn

  
  best_k = get_best_k(trainX, trainY, 10)
  
  train_pred = knn(trainX, trainX, trainY , k = best_k)
  err$knn.train[i] = sum(train_pred != trainY) / nrow(trainX)
  test_pred = knn(trainX, testX, trainY, k = best_k)
  err$knn.test[i] = sum(test_pred != testY) / nrow(testX)
  
}

colors = c("orange3", "khaki4")    #**************************#
boxplot(err,
        xlab = c("Training Data", "Testing Data"),
        col = colors,
        main = "Simulating Train/Testing Error with Various Procedures")
legend('bottomright', 
       c("Training Data", "Testing Data"),
       col = colors,
       bty = "a",
       pch = 19,
       pt.cex = .75,
       horiz = F, 
       inset = c(0.1, 0.1))





## example 1
m1 = c(1, 0)
m0 = c(0, 1)

s = 1
n = 100
p = 2
traindata = matrix(rnorm(2*n*p), 2*n, p)*s + 
  rbind(matrix(rep(m1, n), nrow=n, byrow = T), 
        matrix(rep(m0, n), nrow=n, byrow = T))

dim(traindata)
Ytrain = factor(c(rep(1, n), rep(0, n)))

sqrt(1/5)

sample(1:5, 20, replace = T)

as.numeric(Ytrain)

mean(traindata[n:200, 1])