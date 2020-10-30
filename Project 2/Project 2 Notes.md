# Project 2 Notes

Lower score is better!  Trying to beat 1630

"average of the WMAE over the 10 folds"

Predicting on a each department in each store

Can combine different models to use on different folds if necessary

### Train/Test Sets

Only uses the training data from the original dataset but still splits it up into training and test sets

The test data set is split into 10 "folds" of the training data .  But not used like typical cross-validation folds.  This time is used as small chunks for future predictions.  Test is split up into ten 2-month chunks where you use the previous information to predict the next two months sales data.

Don't actually use fold 10.  It is only used to get our error rate.  For the last prediction (t = 10) we use the training data and all previous folds to predict on fold 10 data

Because it does a left join in the evaluation section on new_train we don't need to worry about making a prediction where there isn't data for that store/dept for that test fold it will automatically get dropped.  However if we don't make a prediction on a given store/dept it replaces NAs with 0s so we will take a large hit for every one (46000 - 0)  

There are 81 departments

And 45 stores

Lots of the store/depts are missing records.  For example during the training year {store 42 dept 23} only has 1 record for the whole year

However 85% of the stores/depts have 'full records' (56 records) in the training data

### Scoring Mechanism

Uses *WMAE* weighted mean average absolute error

Weights holiday scores 5x as much then 

### Allowed packages:

**R packages**:
dplyr, tidyr, reshape2
forcast, lubridate, tidyverse

### Be ware of:

`train`, `test`, and `t` and `train_new` are globally accessible variables

Fold 5 is apparently difficult

Have to update `train` by adding `new_train` onto it.  They suggest using `<<-` for whatever reason.

## Report

**In your report please include the WMAE per fold as well as the overall average**.