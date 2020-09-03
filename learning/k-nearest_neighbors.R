## K-Nearest Neighbors
library(class)
??knn # Man page

attach(Smarket)
Xlag=cbind(Lag1, Lag2)
train=Year<2005

# Manipulate the k value used to see how it effects the accuracy
knn_pred=knn(Xlag[train,], Xlag[!train,], Direction[train],k=3)
table(knn_pred, Direction[!train])
mean(knn_pred==Direction[!train])
