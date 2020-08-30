##### assignment 1 test code
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


#repeat process 20x
sims = 2    #*******************#
lm_error = list(train = rep(0.0, sims), test = rep(0.0, sims))

for(i in 1:sims){
  samples = generate_samples()
  
  #slr
  #train data
  slr_mdl = lm(as.numeric(Ytrain) - 1 ~ ., data = samples$train)
  slr_train_pred = as.numeric(slr_mdl$fitted.values > 0.5)
  ###need to switch these to iterator counter
  lm_error$train[i] = sum(slr_train_pred != samples$train$Ytrain) / nrow(samples$train)
  #test data
  test_pred = ifelse(predict(slr_mdl, newdata = samples$test) > 0.5, 1, 0)
  lm_error$test[i]  = sum(test_pred != samples$test$Ytest) / nrow(samples$test)
  
  #quadratic regression
  quadratic_mdl = lm((as.numeric(Ytrain) - 1) ~ V2 + V3 + I(V2^2) + I(V3^2), data = samples$train)
  }


#



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