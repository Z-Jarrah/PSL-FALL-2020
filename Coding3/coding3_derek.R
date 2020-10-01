#compute leverages 
leverages = function(x1, sp){
  n = length(x1)
  lev = rep(0, n)
  S = matrix(0, n, n)
  
  for(i in 1:n){
    y = rep(0, n)
    y[i] = 1
    
    ####not sure if this is correct
    #enp is not correct
    yi = loess(y ~ x1, span = sp, control = loess.control(surface = "direct"))$fitted
    lev[i] = yi[i]
    # S[, i] = yi
  }
  # S = (S + t(S))/2
  # lev = diag(S)
  return(lev)
}

onestep_cv = function(x1, y1, sp){
  llr      = loess(y1 ~ x1, span = sp, control = loess.control(surface = "direct"))
  n        = length(y1)
  fit_vals = llr$fitted
  resids   = residuals(llr) #need this?
  
  #call loocv for the diag of S
  lev = leverages(x1, sp)
  
  #compute CV
  numerator       = y1 - fit_vals
  cv_denomenator  = 1 - lev
  cv_l = (sum((numerator / cv_denomenator)^2))/n
  
  #compute GCV
  gcv_denomenator = 1 - mean(lev)
  gcv_l = (sum((numerator / gcv_denomenator)^2))/n
  
  return(list(cv = cv_l, gcv = gcv_l))
}

my_CV = function(x1, y1, spans){
  m   = length(spans)
  cv  = rep(0, m)
  gcv = rep(0,m)
  
  #test each value of span
  for(s in 1:m){
    cvs = onestep_cv(x1, y1, spans[s])
    cv[s]  = cvs$cv
    gcv[s] = cvs$gcv
  }
  
  return(list(cv = cv, gcv = gcv))
}


# setup
setwd("~/Google Drive/Geek2/UofIll/CS598_PSL/Assignment_3")
my_data = read.csv("Coding3_Data.csv")
spans = seq(from = 0.20, to = 0.90, length = 15)

#for ease of testing
x1 = my_data$x
y1 = my_data$y

#get cv and gcv results
cv_output = my_CV(x1, y1, spans)
results = cbind(Spans = spans, CV = cv_output$cv, GCV = cv_output$gcv)
results

#best span values for cv + gcv
spans[cv_output$cv == min(cv_output$cv)]
spans[cv_output$gcv == min(cv_output$gcv)]
best_sp = spans[cv_output$gcv == min(cv_output$gcv)]

#plot the simulated data, true function and loess model
fx        = seq(0.02, 1, by = 0.02)
true_y    = sin(12 * (fx + 0.2)) / (fx + 0.2)
best_s_mdl= loess(y1 ~ x1, span = best_sp, control = loess.control(surface = "direct"))
fitted_y  = predict(best_s_mdl, newdata = data.frame(x1 = fx))

plot(y1 ~ x1,
     pch = 19,
     cex = 1.5,
     col = "palevioletred1")
lines(fx, true_y, 
      col = "grey", lwd = 2)
lines(fx, fitted_y,
      col = "dodgerblue",
      lty = 4, lwd = 2)

