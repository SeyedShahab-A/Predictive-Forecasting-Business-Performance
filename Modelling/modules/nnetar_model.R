#nnetar_model.R

library(forecast)


fit_and_validate_nnetar <- function(train_data, test_data) {

  model <- nnetar(train_data, P = 1, size = 3, repeats = 1000)
  

  forecast_horizon <- length(test_data)
  forecast_result <- forecast(model, h = forecast_horizon)
  

  accuracy_result <- accuracy(forecast_result, test_data)
  
  return(list(model = model, accuracy = accuracy_result, forecast = forecast_result))
}


one_step_forecast_nnetar <- function(train_data, test_data, xreg = NULL) {

  model <- nnetar(train_data, xreg = xreg)

  forecast_values <- numeric(length(test_data))
  actual_values <- as.numeric(test_data)
  

  for (i in seq_along(test_data)) {
    if (i == 1) {
      # First forecast is based on the training data
      forecast_result <- forecast(model, h = 1, xreg = xreg[1, , drop = FALSE])
    } else {
      # Subsequent forecasts are based on previous data points
      model <- nnetar(c(train_data, test_data[1:(i-1)]), xreg = xreg[1:i, , drop = FALSE])
      forecast_result <- forecast(model, h = 1, xreg = xreg[(i+1), , drop = FALSE])
    }
    forecast_values[i] <- forecast_result$mean
  }
  
  # Calculate accuracy metrics
  test_accuracy <- accuracy(forecast_values, actual_values)
  train_accuracy <- accuracy(model)
  train_accuracy <- train_accuracy[ , !(colnames(train_accuracy) %in% c("MASE", "ACF1"))]
  
  accuracy_matrix <- rbind(train_accuracy, test_accuracy)
  rownames(accuracy_matrix) <- c("Training set", "Test set")
  
  return(list(model = model, accuracy = accuracy_matrix, forecast = forecast_values))
}

