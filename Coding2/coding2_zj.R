library(MASS)
library(glmnet)
library(class)
myData = Boston
names(myData)[14] = "Y"
iLog = c(1, 3, 5, 6, 8, 9, 10, 14);
myData[, iLog] = log(myData[, iLog]);
myData[, 2] = myData[, 2] / 10;
myData[, 7] = myData[, 7]^2.5 / 10^4
myData[, 11] = exp(0.4 * myData[, 11]) / 1000;
myData[, 12] = myData[, 12] / 100;
myData[, 13] = sqrt(myData[, 13]);
X = as.matrix(myData[, -14])
y = myData$Y
lam.seq = c(0.30, 0.2, 0.1, 0.05, 0.02, 0.005)

fit = lm(y~X,data=myData)
summary(fit)

one_var_lasso = function(r, x, lam) {
  xx = sum(x^2)
  xr = sum(r * x)
  b = (abs(xr) - lam/2)/xx
  b = sign(xr) * ifelse(b > 0, b, 0)
  return(b)
}

MyLasso = function(X, y, lam.seq, maxit = 50) {

  # X: n-by-p design matrix without the intercept
  # y: n-by-1 response vector
  # lam.seq: sequence of lambda values
  # maxit: number of updates for each lambda
  # Center/Scale X
  # Center y

  n = length(y)
  p = dim(X)[2]
  nlam = length(lam.seq)

  ##############################
  # YOUR CODE:
  # Record the corresponding means and scales
  # For example,
  # y.mean = mean(y)
  # yc = centered y
  # Xs = centered and scaled X

  y.mean = mean(y)
  yc = y - mean(y)
  Xs = (X - rowMeans(X)) / apply(X,2, FUN = sd)
  ##############################

  # Initialize coefficients vector b and residual vector r
  b = rep(0, p)
  r = yc
  B = matrix(nrow = nlam, ncol = p + 1)

  # Triple nested loop
  for (m in 1:nlam) {
    lam = 2 * n * lam.seq[m]
    for (step in 1:maxit) {
      for (j in 1:p) {
        r = r + (Xs[, j] * b[j])
        b[j] = one_var_lasso(r, Xs[, j], lam)
        r = r - Xs[, j] * b[j]
      }
    }
    B[m, ] = c(0, b)                                                            # Leading 0 to represent y-intercept???
  }

  ##############################
  # YOUR CODE:
  # Scale back the coefficients;
  # Update the intercepts stored in B[, 1]
  
  
  # Update B
  B = t(B)
  # Tip: its got all the coefficients from every run of lambda
  B[1,1:6] = y.mean
  for (row in 1:length(b)){
    
    for (col in 2:length(nlam)){
  
    }
  }

  
  ##############################
  
  return (B)
}

lam.seq = c(0.30, 0.2, 0.1, 0.05, 0.02, 0.005)
lasso.fit = glmnet(X, y, alpha = 1, lambda = lam.seq)
coef(lasso.fit)

myout = MyLasso(X, y, lam.seq, maxit = 50)
rownames(myout) = c("Intercept", colnames(X))
myout

max(abs(coef(lasso.fit) - myout))