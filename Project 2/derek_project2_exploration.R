setwd("~/Google Drive/Geek2/UofIll/CS598_PSL/Github/PSL-FALL-2020/Project 2")

library(lubridate)
library(tidyverse)
library(reshape2)
library(forecast)

library(progress)

# project code -------
mypredict = function() {
  if (t>1){
    train <<- train %>% add_row(new_train)
  }
  
  start_date <- ymd("2011-03-01") %m+% months(2 * (t - 1))
  end_date <- ymd("2011-05-01") %m+% months(2 * (t - 1))
  test_current <- test %>%
    filter(Date >= start_date & Date < end_date) %>%
    select(-IsHoliday)
  
  test_depts <- unique(test_current$Dept)
  tslm_pred  = NULL
  
  for (dept in test_depts) {
    pb$tick()
    
    train_dept_data <- train %>% filter(Dept == dept)
    test_dept_data <- test_current %>% filter(Dept == dept)
    
    train_stores <- unique(train_dept_data$Store)
    test_stores <- unique(test_dept_data$Store)
    
    # no need to consider stores that do not need prediction
    # or do not have training samples
    test_stores <- intersect(train_stores, test_stores)
    if(length(test_stores) < 1){next}
    
    ##### time series using dudes code
    # Dateframe with (num_test_dates x num_stores) rows
    num_stores = length(test_stores)
    test_dates = unique(test_current$Date)
    num_test_dates = length(test_dates)
    test_frame <- data.frame(
      Date=rep(test_dates, num_stores),
      Store=rep(test_stores, each=num_test_dates))
    
    # Create the same dataframe for the training data
    # (num_train_dates x num_stores)
    train_dates <- unique(train$Date)
    num_train_dates <- length(train_dates)
    train_frame <- data.frame(
        Date=rep(train_dates, num_stores),
        Store=rep(test_stores, each=num_train_dates))
    
    # filter for the particular department in the training data
    train_dept_ts <- train %>%
        filter(Dept == dept) %>%
        select(Store, Date, Weekly_Sales)
    
    # Reformat so that each column is a weekly time-series for that
    # store's department.
    # The dataframe has shape (num_train_dates, num_stores)
    train_dept_ts <- train_frame %>%
        left_join(train_dept_ts, by = c('Date', 'Store')) %>%
        spread(Store, Weekly_Sales)
    
    # We create a similar dataframe to hold the forecasts on
    # the dates in the testing window
    test_dept_ts <- test_frame %>%
        mutate(Weekly_Sales = 0) %>%
        spread(Store, Weekly_Sales)
    
    #simple tslm
    tslm_output = tslm.basic(train_dept_ts, test_dept_ts)
    
    #dont try to shift output for depts in only 1 store.  gets wacky
    if(dim(tslm_output)[2] > 2){
      tslm_output = shift(train, tslm_output, shift = 1)}
    
    tslm_simple = flatten_forecast(tslm_output)
    tslm_simple = cbind(tslm_simple, rep(dept, dim(tslm_simple)[1]))
    colnames(tslm_simple) = c(colnames(tslm_simple)[1:2], 'Weekly_Pred', 'Dept')
    tslm_pred <- tslm_pred %>% bind_rows(tslm_simple)
  }

  return(tslm_pred)
}

naive_model<- function(train_ts, test_ts){
    num_forecasts <- nrow(test_ts)
    train_ts[is.na(train_ts)] <- 0
    
    # naive forecast per store
    for(j in 2:ncol(train_ts)){
        store_ts <- ts(train_ts[, j], frequency=52)
        test_ts[, j] <- naive(store_ts, num_forecasts)$mean
    }
    test_ts
}

postprocess_fold5 = function(preds_df){
  print("runing adjust fold 5")
  preds_df = mutate(preds_df, Weekly_Pred_Snaive = ifelse(Dept == 5 & Date == "2011-12-23", Weekly_Pred_Snaive * 0.55, Weekly_Pred_Snaive))
  preds_df = mutate(preds_df, Weekly_Pred_Snaive = ifelse(Dept == 7 & Date == "2011-12-23", Weekly_Pred_Snaive * 0.8, Weekly_Pred_Snaive))
  preds_df = mutate(preds_df, Weekly_Pred_Snaive = ifelse(Dept == 6 & Date == "2011-12-23", Weekly_Pred_Snaive * 0.45, Weekly_Pred_Snaive))
  preds_df = mutate(preds_df, Weekly_Pred_Snaive = ifelse(Dept == 59 & Date == "2011-12-23", Weekly_Pred_Snaive * 0.4, Weekly_Pred_Snaive))
  preds_df = mutate(preds_df, Weekly_Pred_Snaive = ifelse(Dept == 72 & Date == "2011-12-23", Weekly_Pred_Snaive * 0.85, Weekly_Pred_Snaive))
  
  preds_df = mutate(preds_df, Weekly_Pred_Snaive = ifelse(Dept == 5 & Date == "2011-12-30", Weekly_Pred_Snaive * 1.35, Weekly_Pred_Snaive))
  preds_df = mutate(preds_df, Weekly_Pred_Snaive = ifelse(Dept == 7 & Date == "2011-12-30", Weekly_Pred_Snaive * 1.45, Weekly_Pred_Snaive))
  preds_df = mutate(preds_df, Weekly_Pred_Snaive = ifelse(Dept == 82 & Date == "2011-12-30", Weekly_Pred_Snaive * 1.32, Weekly_Pred_Snaive))
  preds_df = mutate(preds_df, Weekly_Pred_Snaive = ifelse(Dept == 72 & Date == "2011-12-30", Weekly_Pred_Snaive * 1.4, Weekly_Pred_Snaive))
  preds_df = mutate(preds_df, Weekly_Pred_Snaive = ifelse(Dept == 1 & Date == "2011-12-30", Weekly_Pred_Snaive * 1.3, Weekly_Pred_Snaive))
  preds_df = mutate(preds_df, Weekly_Pred_Snaive = ifelse(Dept == 14 & Date == "2011-12-30", Weekly_Pred_Snaive * 1.27, Weekly_Pred_Snaive))
  
  return(preds_df)
}

postprocess <- function(train, test, ...){
  # Iterates over the departments and calls shift() on each.
  #
  # args:
  #  train - the training set as returned from raw.train() in util 
  #  test - a reloaded submission or a data frame similar to test,
  #         from raw.test() in util, but with predictions in the 
  #         Weekly_Sales field
  # ... - additional arguments passed to shift()
  #
  # returns:
  #  the data frame input as test, after calling shift on it department-wise
  # if('Id' %in% names(test)){
  #   #This is a saved submission
  #   sales <- test$Weekly_Sales
  #   test <- raw.test()
  #   test$Weekly_Sales <- sales
  # }
  # 
  
  
  
  test.dates <- unique(test$Date)
  num.test.dates <- length(test.dates)
  all.stores <- unique(test$Store)
  num.stores <- length(all.stores)
  test.depts <- unique(test$Dept)
  forecast.frame <- data.frame(Date=rep(test.dates, num.stores),
                               Store=rep(all.stores, each=num.test.dates))
  pred <- test
  pred$Weekly_Sales <- 0
  
  train.dates <- unique(train$Date)
  num.train.dates <- length(train.dates)
  train.frame <- data.frame(Date=rep(train.dates, num.stores),
                            Store=rep(all.stores, each=num.train.dates))
  for(d in test.depts){
    print(paste('dept:', d))
    tr.d <- merge(train.frame,
                 train[train$Dept==d, c('Store','Date')])
    tr.d <- dcast(tr.d, Date ~ Store) 
    fc.d <- merge(forecast.frame,
                 test[test$Dept==d, c('Store', 'Date')])
    fc.d <- dcast(fc.d, Date ~ Store)
    result <- shift(tr.d, fc.d)
    result <- melt(result)
    pred.d.idx <- pred$Dept==d
    pred.d <- pred[pred.d.idx, c('Store', 'Date')]
    pred.d <- merge(pred.d, result)
    pred$Weekly_Sales[pred.d.idx] <- pred.d$value
  }
  pred
}

shift <- function(train, shift_pred, threshold=1.1, shift=2){
  # This function executes a shift of the sales forecasts in the Christmas
  # period to reflect that the models are weekly, and that the day of the week
  # that Christmas occurs on shifts later into the week containing the holiday.
  #
  # NB: Train is actually not used here. 
  
  # args:
  # train - this is an n_weeks x n_stores matrix of values of Weekly_Sales
  #         for the training set within department, across all the stores
  # test - this is a (forecast horizon) x n_stores matrix of Weekly_Sales
  #        for the training set within department, across all the stores
  # threshold - the shift is executed if the mean of Weekly_Sales for weeks
  #          49-51 is greater than that for weeks 48 and 52 by at least
  #          a ratio of threshold
  # shift - The number of days to shift sales around Christmas.
  #         Should be 2 if the model is based on the last year only,
  #         or 2.5 if it uses both years

  num_stores = dim(shift_pred)[2]
  
  s <- ts(rep(0,39), frequency=52, start=c(2011,44))
  idx <- cycle(s) %in% 48:52
  holiday <- as.data.frame(shift_pred[idx, 2:num_stores])
  baseline <- mean(rowMeans(holiday[c(1, 5), ], na.rm=TRUE))
  surge <- mean(rowMeans(holiday[2:4, ], na.rm=TRUE))
  holiday[is.na(holiday)] <- 0
  if(is.finite(surge/baseline) & surge/baseline > threshold){
    shifted.sales <- ((7-shift)/7) * holiday
    shifted.sales[2:5, ] <- shifted.sales[2:5, ] + (shift/7) * holiday[1:4, ]
    shifted.sales[1, ] <- holiday[1, ]
    shift_pred[idx, 2:num_stores] <- shifted.sales
  }
  
  return(shift_pred)
}

# timeseries helper functions -----
flatten_forecast <- function(f_model) {
  f_model %>%
    gather(Store, value, -Date, convert = TRUE)
  #possibly switch to pivot longer
}

# Adds forecasts to the testing dataframe
update_forecast <- function(test_month, dept_preds, dept) {
  dept_preds <- flatten_forecast(dept_preds)
  
  pred.d <- test_month %>%
    filter(Dept == dept) %>%
    select('Store', 'Date') %>%
    left_join(dept_preds, by = c('Store', 'Date'))
  
  pred.d.idx <- test_month$Dept == dept
  pred.d <- test_month[pred.d.idx, c('Store', 'Date')] %>%
    left_join(dept_preds, by = c('Store', 'Date'))
  
  # if (num_model == 1) {
  #   test_month$Weekly_Pred1[pred.d.idx] <- pred.d$value
  # } else if(num_model == 2) {
  #   test_month$Weekly_Pred2[pred.d.idx] <- pred.d$value
  # } else {
  #   test_month$Weekly_Pred3[pred.d.idx] <- pred.d$value
  # }
  
  test_month
}

# update forecasts in the global test dataframe
update_test <- function(test_month) {
  test <<- test %>%
    dplyr::left_join(test_month,
                     by = c('Date', 'Store', 'Dept', 'IsHoliday')) %>%
    mutate(Weekly_Pred1 = coalesce(Weekly_Pred1.y, Weekly_Pred1.x)) %>%
    mutate(Weekly_Pred2 = coalesce(Weekly_Pred2.y, Weekly_Pred2.x)) %>%
    mutate(Weekly_Pred3 = coalesce(Weekly_Pred3.y, Weekly_Pred3.x)) %>%
    select(-Weekly_Pred1.x, -Weekly_Pred1.y,
           -Weekly_Pred2.x, -Weekly_Pred2.y,
           -Weekly_Pred3.x, -Weekly_Pred3.y)
}

# Forecasts out the last observation in the training data
naive_model<- function(train_ts, test_ts){
  num_forecasts <- nrow(test_ts)
  train_ts[is.na(train_ts)] <- 0
  
  # naive forecast per store
  for(j in 2:ncol(train_ts)){
    store_ts <- ts(train_ts[, j], frequency=52)
    test_ts[, j] <- naive(store_ts, num_forecasts)$mean
  }
  test_ts
}

# simple TSLM model based prediction
tslm.basic <- function(train, test){
  # Computes a forecast using linear regression and seasonal dummy variables
  #
  # args:
  # train - A matrix of Weekly_Sales values from the training set of dimension
  #         (number of weeks in training data) x (number of stores)
  # test - An all-zeros matrix of dimension:
  #       (number of weeeks in training data) x (number of stores)
  #       The forecasts are written in place of the zeros.
  #
  # returns:
  #  the test(forecast) data frame with the forecasts filled in 
  horizon <- nrow(test)
  train[is.na(train)] <- 0
  # first column is the date for each of the stores
  for(j in 2:ncol(train)){
    # grab a single store and create a 52 week time series
    s <- ts(train[, j], frequency=52)
    # create model based on long term trends and weekly 'seasons
    model <- tslm(s ~ trend + season)
    # forecast out desired number of weeks (testing data 'fold')
    fc <- forecast(model, h=horizon)
    #fill in each of the test weeks based on the mean forecast score
    test[, j] <- as.numeric(fc$mean)
  }
  test
}

# evaluation code (dont need for actual submission) ----
# source("mymain.R")
train = suppressMessages(read_csv('train_ini.csv'))
test = suppressMessages(read_csv('test.csv'))

num_folds = 10
wae = rep(0, num_folds)
wae.snaive = rep(0, num_folds)
wae.tslm = rep(0, num_folds)
wae.avg  = rep(0, num_folds)

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

