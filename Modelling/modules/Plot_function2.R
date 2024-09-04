# Load necessary libraries
library(forecast)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(readr)

plot_forecasts <- function(time_series, forecast_result, cost_centre) {
  
  # Plot
  time_series_updated <- window(time_series, end = c(2024, 3))

  forecast_result$mean <- window(forecast_result$mean, end = c(2024, 3))
  forecast_result$lower <- window(forecast_result$lower, end = c(2024, 3))
  forecast_result$upper <- window(forecast_result$upper, end = c(2024, 3))
  
  
  p <- autoplot(time_series_updated) +
    autolayer(forecast_result, PI = TRUE, alpha = 0.2) +  # Adjust transparency with alpha
    labs(title = cost_centre, x = "Time", y = "Value") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  return(p)
}

plot_forecasts_all <- function(results, model_type, division, variable_name) {
  plot_list <- list()
  
  filtered_results <- results[results$Division == division, ]
  
  for (i in seq_along(filtered_results$model_results)) {
    # Create short cost centre name (PM 1, PM 2, ...)
    cost_centre <- paste("PM", i)
    
    # Extract model and forecast results
    model_result <- filtered_results$model_results[[i]][[variable_name]][[model_type]]$model
    forecast_result <- filtered_results$model_results[[i]][[variable_name]][[model_type]]$forecast
    
    # Extract the entire time series data
    time_series <- ts(filtered_results$model_results[[i]][[variable_name]]$ts_data, start = start(model_result$x), frequency = frequency(model_result$x))
    
    plot_list[[i]] <- plot_forecasts(time_series, forecast_result, cost_centre)
  }
  
  if (length(plot_list) > 0) {
    do.call("grid.arrange", c(plot_list, ncol = 3, nrow = ceiling(length(plot_list) / 3)))
  } else {
    message("No plots to display.")
  }
}
