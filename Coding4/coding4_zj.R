library(mclust)

# initial values
# Just for testing
pi1 = 0.5
pi2 = 0.5
mu1 = -0.01
mu2 = 0.01
sigma1 = sqrt(0.01)
sigma2 = sqrt(0.02)

Estep <- function (data , G , para ) {
  # Your Code
  # Compute 
  
  # Return the n-by -G probability matrix
  
}
Mstep <- function (data , G , para , post.prob ) {
  # Your Code
  # Return the updated parameters
}
myEM <- function (data , itmax , G , para ) {
  # itmax: num of iterations
  # G:     num of components
  # para:  list of parameters (prob, mean, Sigma)
  for(t in 1: itmax ) {
    post.prob <- Estep (data , G , para )
    para <- Mstep (data , G , para , post.prob )
  }
  return ( para )
}

# ==============================================================================
# ==============================================================================
# ==============================================================================
# Debugging/ Sandbox

# ==============================================================================



n <- nrow ( faithful )
# initialize parameters
set.seed(9618)
#set.seed(234)
Z <- matrix (0 , n , 2)
Z [ sample (1: n , 120) , 1] <- 1
Z [ , 2] <- 1 - Z [ , 1]

ini0 <- mstep( modelName ="EEE", faithful , Z )$parameters

para0 <- list ( prob = ini0$pro, 
                mean = ini0$mean,
                Sigma = ini0$variance$Sigma )

# Output from my EM alg
myEM ( data = faithful , itmax =10 , G =2 , para = para0 )

# Output from mclust
Rout <- em ( modelName = "EEE", data = faithful ,
             control = emControl( eps =0 , tol =0 , itmax = 10) ,
             parameters = ini0 )$parameters

list ( Rout$pro , Rout$mean , Rout$variance$Sigma )
