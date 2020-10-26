# Project 2 Notes

Lower score is better!  Trying to beat 1630

### Train/Test Sets

Only uses the training data from the original dataset but still splits it up into training and test sets

The test data set is split into 10 "folds" of the training data .  But not used like typical cross-validation folds.  This time is used as small chunks for future predictions.  Test is split up into ten 2-month chunks where you use the previous information to predict the next two months sales data.

Don't actually use fold 10.  It is only used to get our error rate.  For the last prediction (t = 10) we use the training data and all previous folds to predict on fold 10 data

### Scoring Mechanism

Uses *WMAE* weighted mean average absolute error

Weights holiday scores 5x as much then 

### Allowed packages:

**R packages**:
dplyr, tidyr, reshape2
forcast, lubridate, tidyverse