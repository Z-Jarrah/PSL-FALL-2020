
weights = sample(x = c(1,5), size = 10, replace = T, prob = c(.8,.2))

actuals = rnorm(n = 10, 1000, sd = 100)
preds = actuals - rnorm(n = 10, mean = 0, sd = 100)

abs(actuals - preds)

table(train)

##################
######project code
# not all depts need prediction
mypredict = function(){
test_depts <- unique(test_fold$Dept)
test_pred <- NULL
print("Debug 1")
for(dept in test_depts) {
  train_dept_data <- train %>% filter(Dept == dept)
  test_dept_data <- test_fold %>% filter(Dept == dept)
  
  # no need to consider stores that do not need prediction
  # or do not have training samples
  train_stores <- unique(train_dept_data$Store)
  test_stores <- unique(test_dept_data$Store)
  test_stores <- intersect(train_stores, test_stores)
  print("Before inner for loop")
  for (store in test_stores) {
    tmp_train <- train_dept_data %>%
      filter(Store == store) %>%
      mutate(Wk = ifelse(year(Date) == 2010, week(Date) - 1, week(Date))) %>%
      mutate(Yr = year(Date))
    tmp_test <- test_dept_data %>%
      filter(Store == store) %>%
      mutate(Wk = ifelse(year(Date) == 2010, week(Date) - 1, week(Date))) %>%
      mutate(Yr = year(Date))
    
    tmp_train$Wk = factor(tmp_train$Wk, levels = 1:52)
    tmp_test$Wk = factor(tmp_test$Wk, levels = 1:52)
    
    train_model_matrix <- model.matrix( ~ Yr + Wk, tmp_train)
    test_model_matrix <- model.matrix( ~ Yr + Wk, tmp_test)
    mycoef <- lm(tmp_train$Weekly_Sales ~ train_model_matrix)$coef
    mycoef[is.na(mycoef)] <- 0
    tmp_pred <- mycoef[1] + test_model_matrix %*% mycoef[-1]
    
    tmp_test <- tmp_test %>%
      mutate(Weekly_Pred = tmp_pred[, 1]) %>%
      select(-Wk,-Yr)
    test_pred <- test_pred %>% bind_rows(tmp_test)
  }
  print("Out of inner for loop")
}
return(test_pred)
}
##################
######setup files - wont be needed for actual submission
library(lubridate)
library(tidyverse)

raw_data   = readr::read_csv('train.csv')
dates = raw_training$Date
start_date  = ymd("2010-02-01")
end_date    = start_date + months(13)

#setup training/test split
train_ids = which(train_dates >= start_date & train_dates < end_date)
train = raw_training[train_ids, ]
test  = raw_training[-train_ids, ]

#write datafiles
write_csv(train, 'train_ini.csv')
write_csv(select(test, -Weekly_Sales), "test.csv")
n_folds = 10
test_dates = dates[-train_ids]

for(i in 1:n_folds){
  #filter for fold dates
  start_date = ymd("2011-03-01") + months(2 * (i-1))
  end_date   = ymd("2011-05-01") + months(2 * (i-1))
  
  test_chunk = filter(test, Date>= start_date & Date < end_date)
  write_csv(test_chunk, paste0('fold_', i, '.csv'))
}


##################
######evaluation code
######dont need for actual submission only mymain.R
source("mymain.R")
train = suppressMessages(read_csv('train_ini.csv'))
test = suppressMessages(read_csv('test.csv'))

num_folds = 2
wae = rep(0, num_folds)

for(t in 1:num_folds){
  test_pred = mypredict()
  print("past predict")
  
  fold_file = paste0('fold_', t, '.csv')
  new_train = read_csv(fold_file, col_types = cols())
  
  ###doesnt work correctly until the output from mypredict is correctly structured
  print("Debug: Before scoring table")
  scoring_tbl = merge(new_train, test_pred, by = c('Date', 'Store', 'Dept'))
  
  actuals = scoring_tbl$Weekly_Sales
  preds = scoring_tbl$Weekly_Pred
  preds[is.na(preds)] = 0
  weights = if_else(scoring_tbl$IsHoliday, 5, 1)
  wae[t] = sum(weights * abs(actuals - pred)) / sum(weights)
}

print(wae)
mean(wae)
