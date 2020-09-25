lo.lev <- function(x1, sp){
  # x1: feature vector of length n
  # sp: a value for "span"
  
  n = length(x1);
  lev = rep(0, n)
  
  ##############################################
  # YOUR CODE: Compute the diagonal entries of the 
  #            smoother matrix S and 
  #            store it in a vector "lev"
  # Tip: check how we compute the smoother matrix
  #      for smoothing spline models
  
  
  ##############################################
  
  return(lev)
}

onestep_CV <- function(x1, y1, sp){
  
  ##############################################
  #  YOUR CODE: 
  #  1) Fit a loess model y1 ~ x1 with span = sp, and extract 
  #     the corresponding residual vector
  #  2) Call lo.lev to obtain the diagonal entries of S
  #  3) Compute LOO-CV and GCV using formula from lecture notes
  #    [lec_W5_NonlinearRegression.pdf] page 33. 
  ##############################################
  
  return(list(cv = cv, gcv = gcv))
}

myCV <- function(x1, y1, span){
  # x1: feature vector of length n
  # y1: response vector of length n
  # span: a sequence of values for "span"
  
  m = length(span)
  cv = rep(0, m)
  gcv = rep(0, m)
  
  for(i in 1:m){
    tmp = onestep_CV(x1, y1, span[i])
    cv[i] = tmp$cv
    gcv[i] = tmp$gcv
  }
  return(list(cv = cv, gcv = gcv))
}

mydata = read.csv(file = "Coding3_Data.csv")
dim(mydata)

plot(mydata$x, mydata$y, xlab="", ylab="")

span1 = seq(from = 0.2, by = 0.05, length = 15 )
cv.out = myCV(mydata$x, mydata$y, span1)

myout = data.frame(CV = cv.out$cv, 
                   GCV = cv.out$gcv, 
                   span = span1)
myout

myout$span[myout$GCV == min(myout$GCV)]

myout$span[myout$CV == min(myout$CV)]

spangcv.min = 0.5
plot(mydata$x, mydata$y, xlab="", ylab="", col="gray");
fx = 1:50/50;
fy = sin(12*(fx+0.2))/(fx+0.2)
lines(fx, fy, col=8, lwd=2)

f = loess(y ~ x, mydata, span = spangcv.min)
lines(fx, predict(f, data.frame(x = fx), surface = "direct"), 
      lty=2, lwd=2, col="blue")