##
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

#####
csize = 10
s = 1
p = 2

m1 = matrix(rnorm(csize*p), csize, p) * s + cbind(rep(1, csize), rep(0, csize))
m0 = matrix(rnorm(csize*p), csize, p) * s + cbind(rep(0, csize), rep(1, csize))

n = 100
id1 = sample(1:csize, n, replace = T)
id0 = sample(1:csize, n, replace = T)

s = sqrt(1/5)

traindata = matrix(rnorm(2*n*p), 2*n, p)*s  + rbind(m1[id1, ], m0[id0, ]))
Ytrain = factor(c(rep(1, n), rep(0, n)))

lm_model = lm(as.numeric(Ytrain) - 1 ~ traindata)