#bagged Models.R

library(forecast)

fit_and_validate_bagged_arima <- function(train_data, test_data) {

  model <- baggedModel(train_data, fn = auto.arima)
  chosen_method <- "Bagged ARIMA"
  

  forecast_horizon <- length(test_data)
  forecast_result <- forecast(model, h = forecast_horizon)
  
  accuracy_result <- accuracy(forecast_result, test_data)
  
  model$bootstrapped_series <- NULL

  
  return(list(model = model, chosen_method = chosen_method, accuracy = accuracy_result, forecast = forecast_result))
}

one_step_forecast_bagged_arima <- function(train_data, test_data) {

  model <- baggedModel(train_data, fn = auto.arima)
  chosen_method <- "Bagged_ARIMA"
  
  forecast_values <- numeric(length(test_data))
  actual_values <- as.numeric(test_data)
  

  for (i in seq_along(test_data)) {
    if (i == 1) {
      # First forecast is based on the training data
      forecast_result <- forecast(model, h = 1)
    } else {
      # Subsequent forecasts are based on previous data points
      model <- baggedModel(c(train_data, test_data[1:(i-1)]), fn = auto.arima)
      forecast_result <- forecast(model, h = 1)
    }
    forecast_values[i] <- forecast_result$mean
  }
  
  test_accuracy <- accuracy(forecast_values, actual_values)
  train_accuracy <- accuracy(model)
  train_accuracy <- train_accuracy[ , !(colnames(train_accuracy) %in% c("MASE", "ACF1"))]
  
  accuracy_matrix <- rbind(train_accuracy, test_accuracy)
  rownames(accuracy_matrix) <- c("Training set", "Test set")
  
  return(list(model = model, chosen_method = chosen_method, accuracy = accuracy_matrix, forecast = forecast_values))
}


fit_and_validate_bagged_ets <- function(train_data, test_data) {

  model <- baggedETS(train_data)
  chosen_method <- "Bagged_ETS"

  
  forecast_horizon <- length(test_data)
  forecast_result <- forecast(model, h = forecast_horizon)
  
  accuracy_result <- accuracy(forecast_result, test_data)
  
  model$bootstrapped_series <- NULL

  
  return(list(model = model, chosen_method = chosen_method, accuracy = accuracy_result, forecast = forecast_result))
}


one_step_forecast_bagged_ets <- function(train_data, test_data) {

  model <- baggedETS(train_data)
  chosen_method <- "Bagged ETS"
  

  forecast_values <- numeric(length(test_data))
  actual_values <- as.numeric(test_data)
  
  for (i in seq_along(test_data)) {
    if (i == 1) {
      # First forecast is based on the training data
      forecast_result <- forecast(model, h = 1)
    } else {
      # Subsequent forecasts are based on previous data points
      model <- baggedETS(c(train_data, test_data[1:(i-1)]))
      forecast_result <- forecast(model, h = 1)
    }
    forecast_values[i] <- forecast_result$mean
  }
  
  test_accuracy <- accuracy(forecast_values, actual_values)
  train_accuracy <- accuracy(model)
  train_accuracy <- train_accuracy[ , !(colnames(train_accuracy) %in% c("MASE", "ACF1"))]
  
  accuracy_matrix <- rbind(train_accuracy, test_accuracy)
  rownames(accuracy_matrix) <- c("Training set", "Test set")
  
  return(list(model = model, chosen_method = chosen_method, accuracy = accuracy_matrix, forecast = forecast_values))
}

