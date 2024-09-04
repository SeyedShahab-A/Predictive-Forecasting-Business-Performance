#manager_accuracy.R

library(forecast)


calculate_manager_accuracy2 <- function(data, variable, start_index, n = 12) {

  actual_column <- data[[variable]]
  forecast_column <- data[[paste0(variable, "_fcst12")]]
  
  # Use the data points from the start_index to the next n values
  actual_values <- actual_column[start_index:(start_index + n - 1)]
  forecast_values <- forecast_column[start_index:(start_index + n - 1)]
  
  valid_indices <- !is.na(forecast_values) & !is.na(actual_values)
  actual_values <- actual_values[valid_indices]
  forecast_values <- forecast_values[valid_indices]
  
  # If there are no valid data points, return NA for all metrics
  if (length(actual_values) == 0 || length(forecast_values) == 0) {
    return(data.frame(ME = NA, RMSE = NA, MAE = NA, MPE = NA, MAPE = NA, MASE = NA))
  }
  
  accuracy_metrics <- accuracy(forecast_values, actual_values)
  
  return(accuracy_metrics)
}








