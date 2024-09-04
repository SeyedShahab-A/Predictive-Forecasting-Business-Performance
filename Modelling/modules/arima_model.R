# arima_model.R

library(forecast)

fit_and_validate_arima <- function(train_data, test_data) {

  model <- auto.arima(train_data, seasonal = TRUE)
  
  forecast_horizon <- length(test_data)
  forecast_result <- forecast(model, h = forecast_horizon)
  
  accuracy_result <- accuracy(forecast_result, test_data)
  
  start_date <- time(test_data)[1]
  time_index <- seq(from = as.Date(start_date), by = "month", length.out = forecast_horizon)
  
  return(list(model = model, accuracy = accuracy_result, forecast = forecast_result))
}



one_step_forecast_arima <- function(train_data, test_data) {

  model <- auto.arima(train_data)
  
  forecast_values <- numeric(length(test_data))
  actual_values <- as.numeric(test_data)
  
  for (i in seq_along(test_data)) {
    if (i == 1) {

      forecast_result <- forecast(model, h = 1)
    } else {

      forecast_result <- forecast(Arima(c(train_data, test_data[1:(i-1)]), model = model), h = 1)
    }
    forecast_values[i] <- forecast_result$mean
  }
  
  test_accuracy <- accuracy(forecast_values, actual_values)
  train_accuracy <- accuracy(model)
  train_accuracy <- train_accuracy[ , !(colnames(train_accuracy) %in% c("MASE", "ACF1"))]
  
  accuracy_matrix <- rbind(train_accuracy, test_accuracy)
  rownames(accuracy_matrix) <- c("Training set", "Test set")
  
  
  return(list(model = model, accuracy = accuracy_matrix, forecast = forecast_values))
}


