#import and setup train/test data for a specifc train/test set
testIDs = read.table("project1_testIDs.dat")
data    = read.csv("Ames_data.csv", stringsAsFactors = F)

for(j in 1:10) {
  cat("Split ", j, "   ##############", '\n')
  
  #read in splits
  train = data[-testIDs[, j],]
  test  = data[testIDs[, j],-83]
  test_y = data[testIDs[, j], c(1, 83)]
  
  #write train, test, test_j csv's
  write.csv(train, file = "train.csv", row.names = F)
  write.csv(test, file = "test.csv", row.names = F)
  write.csv(test_y, file = "test_y.csv", row.names = F)
  
  #run mymain.r
  source("mymain.R")
  
  #read in submission1.csv and submission2.csv and calculate scores
  test.y = read.csv('test_y.csv')
  names(test.y)[2] = "True_Sale_Price"
  
  pred = read.csv('mysubmission1.txt')
  pred = merge(pred, test.y, by = "PID")
  score1 = sqrt(mean((log(pred$Sale_Price) - log(pred$True_Sale_Price))^2))
  
  pred = read.csv('mysubmission2.txt')
  pred = merge(pred, test.y, by = "PID")
  score2 = sqrt(mean((log(pred$Sale_Price) - log(pred$True_Sale_Price))^2))
  
  #calculate rmse and store
  cat("Model 1: ", score1, "    Model 2: ", score2, "\n")
}
