##Project 1 - mymain.R
#Derek Chapman (derek4)
#Zeed Jarah (zjarrah2)


#must return dataframe with columsn Date, Store, Dept and Weekly_Pred
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

# timeseries helper function
flatten_forecast <- function(f_model) {
  f_model %>%
    gather(Store, value, -Date, convert = TRUE)
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
