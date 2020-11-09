setwd("~/Google Drive/Geek2/UofIll/CS598_PSL/Github/PSL-FALL-2020/Project 2")

library(lubridate)
library(tidyverse)
library(reshape2)
library(forecast)

library(progress)

# evaluation code (dont need for actual submission) ----
source("mymain.R")
train = suppressMessages(read_csv('train_ini.csv'))
test = suppressMessages(read_csv('test.csv'))

num_folds = 10
wae = rep(0, num_folds)

system.time({
  for (t in 1:num_folds) {
    test_pred = mypredict()
    
    fold_file = paste0('fold_', t, '.csv')
    new_train = read_csv(fold_file, col_types = cols())
    scoring_tbl = left_join(new_train, test_pred, by = c('Date', 'Store', 'Dept'))

    actuals = scoring_tbl$Weekly_Sales
    weights = if_else(scoring_tbl$IsHoliday, 5, 1)
    
    preds = scoring_tbl$Weekly_Pred
    preds[is.na(preds)] = 0
    wae[t] = sum(weights * abs(actuals - preds)) / sum(weights)
    print(t)
    print(wae[t])
  }
})

print(wae)
mean(wae)

best = rep(0, 10)
for(w in 1:10){
  best[w] = min(wae.snaive[w], wae.tslm[w], wae.avg[w])
}

# setup the 'fold' files - (wont be needed for actual submission) ----
raw_training   = readr::read_csv('train.csv')
dates = raw_training$Date
start_date  = ymd("2010-02-01")
end_date    = start_date + months(13)

#setup training/test split
train_ids = which(dates >= start_date & dates < end_date)
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

# my scratchpad -----
stores = unique(train$Store)
full_record_count = 0
num_combos = 0

for(s in stores){
  depts = unique(train[train$Store == s, ]$Dept)
  for(d in depts){
    num_combos = num_combos + 1
    count = dim(train[train$Store == s & train$Dept == d, ])[1]
    if(count == 56){full_record_count = full_record_count + 1}
    # print(paste0("store: ", s, "   dept: ", d, "   records = ", count))
  }
}
full_record_count/num_combos

t = 2045+1466.912+ 1449.852+ 1593.998+ 2238.742 + 1677.483+1722.274+ 1428.212+ 1443.960+ 1444.656
t/10

stores = unique(train$Store)
num_depts = rep(0, length(stores))
iter = 1

for(s in stores){
  print(iter)
  iter = iter + 1
  print(paste0("Store ", s, " # of departments: ", dim(unique(train[train$Store == s, "Dept"]))[1]))
}

hist(num_depts, 
     xlab = "",
     xlim = c(60, 80),
     ylab = "",
     breaks = 15,
     border = F,
     col = "cornflowerblue",
     main = "Number of Departments in a Store")


# analyze how far off predictions in 5/7th fold are -------
scoring_tbl$off_by = scoring_tbl$Weekly_Sales - scoring_tbl$Weekly_Pred
scoring_tbl$percent_off = scoring_tbl$off_by / scoring_tbl$Weekly_Sales

depts = sort(unique(scoring_tbl$Dept))
tble = data.frame()

for(dept in depts){
  dept_mean_off = round(mean(scoring_tbl[scoring_tbl$Date == "2012-03-01" & scoring_tbl$Dept == dept, ]$percent_off), 3)
  num_stores = nrow(scoring_tbl[scoring_tbl$Date == "2012-03-01" & scoring_tbl$Dept == dept, ])
  volume_off = round(sum(scoring_tbl[scoring_tbl$Date == "2012-03-01" & scoring_tbl$Dept == dept, ]$off_by), 0)

  details = c(as.character(dept), num_stores, dept_mean_off, volume_off)
  tble = rbind(tble, details)
  
  # if(is.finite(dept_mean_off) & dept_mean_off > 0.15){
  #   print(paste0("dept: ", dept, "   with ", num_stores, " stores ", " is off by: ", dept_mean_off))}
}

colnames(tble) = c("dept", "#_stores", "mean_perc_off", "volume_off_by")
row_idx = order(as.double(tble[, "volume_off_by"]))
tble[row_idx, ]

