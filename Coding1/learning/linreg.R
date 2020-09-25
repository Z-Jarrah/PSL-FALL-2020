# Taken from Chapter 3 Lab: linear Regression
library(MASS)
library(ISLR)

### Simple linear regression
#names(Boston)
#?Boston

plot(medv~lstat, Boston)

fit1 = lm(medv~lstat, data=Boston)
#fit1
summary(fit1)
abline(fit1, col='red')

names(fit1)
confint(fit1)

predict(fit1, data.frame(lstat=c(5,10,15)), interval = "confidence")


### Multiple linear regression
fit2=lm(medv~lstat+age, data=Boston) # Use lstat and age
summary(fit2)

fit3=lm(medv~. , Boston) #Use all variables except medv(predictor variable)
summary(fit3)

par(mfrow=c(2,2))
plot(fit3)


# Fit a model (fit3) 
# Updated by using same response but removing age and indus
fit4=update(fit3,~.-age-indus)
summary(fit4)

### Nonlinear terms and Interactions
fit5=lm(medv~lstat*age, Boston) #Interaction with lstat and age
summary(fit5)

#lstat with quadratic; put inside an identity function
fit6=lm(medv~lstat +I(lstat^2), Boston)
summary(fit6)

attach(Boston)
par(mfrow=c(1,1))
plot(medv~lstat)

points(lstat, fitted(fit6),col="red", pch=20)

fit7=lm(medv~poly(lstat,4))
points(lstat, fitted(fit7), col="blue", pch=20)

# Plots all the plotting characters 1 thru 20; cex=2 doubles the size by 2
plot(1:20, 1:20, pch=1:20, cex=2)


### Misc learning objectives
fix(Carseats)
names(Carseats)
summary(Carseats)
fitseat1 = lm(Sales~.+Income:Advertising+Age:Price, Carseats)
summary(fitseat1)
contrasts(Carseats$ShelveLoc)

### Writing R Functions
#regplot=function(x,y){
#  fit=lm(y~x)
#  plot(x,y)
#  abline(fit, col="red")
#}

attach(Carseats)
regplot(Price, Sales)

### Function but using the ... construct
regplot=function(x,y,...){
  fit=lm(y~x)
  plot(x,y, ...)
  abline(fit, col="red")
}

regplot(Price, Sales, xlab="Price", ylab="Sales", col="blue", pch=20)



















