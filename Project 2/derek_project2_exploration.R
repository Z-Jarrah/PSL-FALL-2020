
weights = sample(x = c(1,5), size = 10, replace = T, prob = c(.8,.2))

actuals = rnorm(n = 10, 1000, sd = 100)
preds = actuals - rnorm(n = 10, mean = 0, sd = 100)

abs(actuals - preds)

table(train)

##################
######project code

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