require(ISLR)
require(MASS)

## Linear Discriminant Analysis using stock market data
lda_fit=lda(Direction~Lag1+Lag2, data=Smarket, subset=Year<2005)
lda_fit
plot(lda_fit)


Smarket_2005=subset(Smarket, Year==2005)
lda_pred=predict(lda_fit, Smarket_2005)
lda_pred[1:5,] # This is not correct. This list can be cast as a data frame 
class(lda_pred)

data.frame(lda_pred)[1:5,]
table(lda_pred$class, Smarket_2005$Direction)
mean(lda_pred$class==Smarket_2005$Direction)
