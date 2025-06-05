setwd("C:/Users/iwilf/OneDrive/Documents/Kaggle/KaggleCoin")

test <- read.csv("test.csv")
train <- read.csv("train.csv")

library(forecast)
library(ggplot2)
library(lmtest)


ggplot(data= train, aes(x=day_id, y=close)) +
  geom_point() 

# A function to turn any data set into a first differenced data set

difference_dataset <- function(data){
  data_diff <- data # copies data set
  names <- colnames(data_diff) # gets a string to iterate over
  for(name in names) {
    if (is.numeric(data_diff[[name]])){ # checks if numeric
    data_diff[[name]] <- c(NA, diff(data_diff[[name]])) #updates dataset
    } # the above needs an NA because the 1st row goes away on 1st differences
  }
  data_diff <- data_diff[-1,] # removes first observations
  return(data_diff)
}

#must save to a variable 

First_Diff <- difference_dataset(train)

# Function to check stationarity of every series in a data frame


stationary <- function(data) {
  names <- colnames(data) # gets names to iterate over
  vec <- c() # empty vector
  
  for (name in names) {
    if (is.numeric(data[[name]])) {
      vec <- c(vec, ndiffs(data[[name]])) 
    }
  }
  return(vec)
}

# checking stationarity of base data frame

stationary(train)

# double checking stationarity of first difference data frame

stationary(First_Diff)

# fit an ARIMA model for the closing data 

model_arima_close <- auto.arima(train$close)

# this uses auto regressive and moving average components to forecast

# fit an exponential smoothing model 

model_ets_close <- ets(train$close)

# forecast future values based on weighted averages of past values

# Forecast

forecast_arima_close <- forecast(model_arima_close, h=6)
forecast_ets_close <- forecast(model_ets_close, h = 6)

# plot each forecast 

plot(forecast_arima_close)
plot(forecast_ets_close)

# evaluate accuracy 

accuracy(forecast_arima_close)
accuracy(forecast_ets_close)

# saving the forecasts into a vector for submission 

forecast_vector_ets_close <- tail(forecast_ets_close$x, 6)
#adding vector into a data frame for submission 

test$close <- forecast_vector_ets_close

# save it to a csv for submission purposes

write.csv(test, file = "submission.csv", row.names = FALSE)

# forecasting with volumn trends in mind using ARIMA

   # testing granger casuality 

grangertest(train$close ~ train$volume, order=1)  # shows low predictive casuality


volumn_arima_close <- auto.arima(train$close, xreg = train$volume)
forecast_volumn_trends <- forecast(volumn_arima_close, h=6, xreg=tail(train$volume, 6))
plot(forecast_volumn_trends)
accuracy(forecast_volumn_trends)

# saving to upload to kaggle

forecasted_values_volumn <- tail(forecast_volumn_trends$x, 6)
test$close <- forecasted_values_volumn
write.csv(test, file = "submissionvolumn.csv", row.names = FALSE)


#trying granger casuality with other variables 

grangertest(train$close ~ train$open, order=1) # no causality
grangertest(train$close ~ train$high, order=1) # high causality
grangertest(train$close ~ train$high, order=2) # high causality
grangertest(train$close ~ train$high, order=3) # high causality
grangertest(train$close ~ train$low, order=1) # no causality

# forecasting with high trade value in model 

   # lagging the high price 

train$lagged_high <- c(NA, train$high[-nrow(train)])


high_arima_close <- auto.arima(train$close, xreg = train$lagged_high)
forecast_high_trends <- forecast(volumn_arima_close, h=6, xreg=tail(train$high, 6))
plot(forecast_high_trends, xlim=c(33000,35200))
accuracy(forecast_high_trends) # accuracy improved 


forecasted_values_high <- tail(forecast_high_trends$x, 6)
test$close <- forecasted_values_high
write.csv(test, file = "submissionhigh.csv", row.names = FALSE)

forecasted_values_volumn == forecasted_values_high

