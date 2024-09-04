#grnn.R
library(tsfgrnn)

fit_and_validate_grnn <- function(train_data, test_data) {

  h = length(test_data)
  pred <- grnn_forecasting(train_data, lags = 1:6, h = h, transform = 'additive')
  
  forecast_result <- pred$prediction
  
  accuracy_result <- accuracy(forecast_result, test_data)
  
  return(list(
    model = pred, 
    forecast = forecast_result,
    accuracy = accuracy_result
  ))
}

