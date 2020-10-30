library(mclust)

densityMvNorm = function (x, mean, sigma, log = FALSE) 
{
  mean = as.vector(mean)

  if (missing(sigma)) sigma = diag(ncol(x))
  
  if (NCOL(x) != NCOL(sigma)) {
    stop("x and sigma have non-conforming size")
  }
  if (!isSymmetric(sigma, tol = sqrt(.Machine$double.eps), 
                   check.attributes = FALSE)) {
    stop("sigma must be a symmetric matrix")
  }
  if (length(mean) != NROW(sigma)) {
    print(NCOL(mean))
    print(NROW(sigma))
    stop("mean and sigma have non-conforming size")
  }
  
  ## invert matrix before hand so only do this once
  prec = solve(sigma)
  distval = mahalanobis(x, center = mean, cov = prec, inverted = TRUE)
  logdet = sum(log(eigen(sigma, symmetric = TRUE, only.values = TRUE)$values))
  logretval = -(ncol(x) * log(2 * pi) + logdet + distval)/2
  
  if (log) 
    return(unname(logretval))
  else 
    return(exp(logretval))
}

Estep <- function (data, G, para) {
  prob1_vec = para$prob[1]*densityMvNorm(faithful, para$mean[,1], para$Sigma)/
    (para$prob[1]*densityMvNorm(faithful, para$mean[,1], para$Sigma) + para$prob[2]*densityMvNorm(faithful, para$mean[,2], para$Sigma))

  # Return the n-by -G probability matrix
  return (cbind(prob1_vec, 1-prob1_vec))
}

Mstep <- function (data, G, para, post.prob) {
  prob = apply(post.prob, 2, mean)
  
  rnk_Xn1 = post.prob[,1]*faithful
  mean1   = apply(rnk_Xn1, 2,sum)/apply(post.prob,2,sum)[1]
  rnk_Xn2 = post.prob[,2]*faithful
  mean2   = apply(rnk_Xn2, 2,sum)/apply(post.prob,2,sum)[2]
  updated_mean = cbind(mean1, mean2)
  
  mat_temp1 = matrix(rep(0, 4), nrow=2)
  
  for (i in 1:nrow(faithful)) {
    x1 = post.prob[i,1] * (t(faithful[i,]-mean1) %*% t(t(faithful[i,]-mean1)))
    x2 = post.prob[i,2] * (t(faithful[i,]-mean2) %*% t(t(faithful[i,]-mean2)))
    
    mat_temp1 = mat_temp1 + x1 + x2
  }
  
  covar_matrix = mat_temp1/nrow(faithful)

  # Return the updated parameters
  return(list(prob = prob, mean = updated_mean, Sigma = covar_matrix))
}

myEM <- function (data, itmax, G, para){
  for(t in 1: itmax ) {
    post.prob = Estep (data , G , para )
    para = Mstep (data, G, para, post.prob)
  }
  return(para)
}

# ==============================================================================
# Debugging/ Sandbox
set.seed(9618)

# initialize parameters
n = nrow(faithful)
Z = matrix (0 , n , 2)
Z[sample(1:n, 120), 1] = 1
Z[, 2] = 1 - Z[, 1]

ini0 <- mstep( modelName ="EEE", faithful , Z )$parameters

para0 <- list ( prob = ini0$pro, 
                mean = ini0$mean,
                Sigma = ini0$variance$Sigma )

# Output from my EM alg
my_out = myEM(data = faithful, itmax = 10, G= 2, para = para0)

# Output from mclust
Rout = em ( modelName = "EEE", data = faithful ,
             control = emControl( eps =0 , tol =0 , itmax = 10) ,
             parameters = ini0 )$parameters

probs = cbind(my_out$prob, Rout$pro)
colnames(probs) = c("My EM", "mclust")
rownames(probs) = c("Cluster 1", "Cluster 2")
probs
