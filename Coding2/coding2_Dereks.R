#PSL Coding Assignment #2
seed = 0721
set.seed(seed)

library(MASS)
library(glmnet)

#preprocess Housing Data
{
house_data = Boston
colnames(house_data)[14] = "Y"
ilog = c(1, 3, 5, 6, 8, 9, 10, 14)
house_data[, ilog] = log(house_data[, ilog])
house_data[, 2]  = house_data[, 2] / 10
house_data[, 7]  = house_data[, 7]^2.5 / 10^4
house_data[, 11] = exp(0.4 * house_data[, 11]) / 1000
house_data[, 12] = house_data[, 12] / 100
house_data[, 13] = sqrt(house_data[, 13])

#setup Y ~ X and lambda values
X = as.matrix(house_data[, -14])
y = house_data[, 14]
lam_seq = c(0.3, 0.2, 0.1, 0.05, 0.02, 0.005)
}

#My Lasso
my_lasso = function(X, y, lam_seq, maxiter = 50, cold_start = F){
  n = dim(X)[1]
  p = dim(X)[2]
  num_lambda = length(lam_seq)
  
  #center and scale as needed
  y_mean = mean(y)
  y_centered = y - mean(y)
  
  X_means = colMeans(X)
  X_centered = t(t(X) - X_means)
  Xsds = apply(X, 2, sd) * sqrt((n-1)/n)
  Xs = t(t(X_centered)/Xsds)
  
  #Initalize coef and residuals vectors
  b = rep(0, p)
  if(cold_start == T){r = rep(0, n)} else{r = y_centered}
  # r = ifelse(cold_start == T, list(rep(0, n)), list(y_centered))
  # r = y_centered
  B = matrix(nrow = num_lambda, ncol = p + 1)
  
  #scale the lambda to the one used in glmnet to compare
  lam_seq_scaled = lam_seq * 2 * n
  
  for(l in 1:num_lambda){  #for each lambda value
    #-------my addition
    # b = rep(0, p)
    for(i in 1:maxiter){  #for each iteration of our step value
      for(j in 1:p){      #for each beta
        #add in the effect of the current parameter
        r = r + (Xs[, j] * b[j])
        #perform one-variable-lasso
        b[j] = one_var_lasso(r, Xs[, j], lam_seq_scaled[l])
        # remove the effect of the current variable based on its new value
        r = r - (Xs[, j] * b[j])
      }
    }
    B[l, ] = c(0, b)
  }
  
  #unscale coefficients
  sds = c(0, apply(X, 2, sd))
  sds_matrix = rbind(sds, sds, sds, sds, sds, sds)
  Bs = B/sds_matrix
  
  #update intercepts in Bs[, 1]
  for(each in 1:num_lambda){
    Bs[each, 1] = y_mean - sum(X_means * Bs[each, 2:14], na.rm = T)
  }
  
  return(t(Bs))
}

one_var_lasso = function(r, x, lam){
  xx = sum(x^2)
  xr = sum(r*x)
  b = (abs(xr) - lam/2) / xx
  b = sign(xr) * ifelse(b > 0, b, 0)
  return(b)
  }


official_fit = glmnet(X, y, alpha = 1, lambda = lam_seq)
my_lasso_fit_cold = my_lasso(X, y, lam_seq, 50, cold_start = T)
my_lasso_fit_warm = my_lasso(X, y, lam_seq, 50, cold_start = F)
rownames(my_lasso_fit) = c("Intercept", colnames(X))

round(coef(official_fit), 3)
round(my_lasso_fit, 3)

l1_error = abs(sum(coef(official_fit) - my_lasso_fit))

cat("Off by:", l1_error)

ifelse(l1_error < 0.005, 
       print("Send it!"), 
       print("Little off...."))

