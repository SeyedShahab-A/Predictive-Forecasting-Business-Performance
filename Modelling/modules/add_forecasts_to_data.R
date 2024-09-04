library(dplyr)

add_forecasts_to_data <- function(data, results, variable, model_type, one_step = FALSE) {

  for (i in seq_along(results$Cost_Centre)) {
    cost_centre <- results$Cost_Centre[i]
    forecasts <- results$model_results[[i]][[variable]]
    
    suffix <- if (one_step) "one_step_forecast" else "forecast"
    forecast_column <- paste(variable, model_type, suffix, sep = "_")
    
    # Initialize the forecast column if it doesn't exist
    if (!(forecast_column %in% names(data))) {
      data[[forecast_column]] <- NA
    }
    
    # Get the forecast values for the specified model
    if (one_step) {
      model_forecasts <- forecasts[[model_type]]$forecast
    } else {
      model_forecasts <- forecasts[[model_type]]$forecast$mean
    }
    
    # Determine the start index of the test data
    train_size <- length(forecasts[[model_type]]$model$x)
    test_size <- length(model_forecasts)
    start_index <- which(data$Cost_Centre == cost_centre)[train_size + 1]
    end_index <- start_index + test_size - 1
    

    data[start_index:end_index, forecast_column] <- model_forecasts
  }
  
  return(data)
}


library(dplyr)

add_all_forecasts_to_data <- function(data, results, one_step = FALSE) {

  for (i in seq_along(results$Cost_Centre)) {
    cost_centre <- results$Cost_Centre[i]
    forecast_results <- results$model_results[[i]]
    

    for (variable in names(forecast_results)) {

      if (variable %in% c("ts_data", "test_data", "test_size")) next
      
      forecasts <- forecast_results[[variable]]
      

      for (model_type in names(forecasts)) {

        if (model_type %in% c("ts_data", "test_data", "test_size")) next
        if (is.null(forecasts[[model_type]])) next
        

        suffix <- if (one_step) "one_step_forecast" else "forecast"
        forecast_column <- paste(variable, model_type, suffix, sep = "_")
        
        # Initialize the forecast column if it doesn't exist
        if (!(forecast_column %in% names(data))) {
          data[[forecast_column]] <- NA
        }
        
        if (one_step) {
          model_forecasts <- forecasts[[model_type]]$forecast
        } else {
          model_forecasts <- forecasts[[model_type]]$forecast
        }
        
        # Determine the start index of the test data
        train_size <- length(forecasts[[model_type]]$model$model$ts)
        test_size <- length(model_forecasts)
        start_index <- which(data$Cost_Centre == cost_centre)[train_size + 1]
        end_index <- start_index + test_size - 1
        
        # Update the forecast column with the forecast values
        data[start_index:end_index, forecast_column] <- model_forecasts
      }
    }
  }
  
  return(data)
}

add_var_forecasts_to_data <- function(data, var_forecasts_df) {

  for (i in seq_along(var_forecasts_df$Date)) {
    forecast_date <- var_forecasts_df$Date[i]
    cost_centre <- var_forecasts_df$Cost_Centre[i]
    
    # Match the forecast row to the data based on Date and Cost_Centre
    matching_rows <- which(data$Date == forecast_date & data$Cost_Centre == cost_centre)
    
    # For each variable in the VAR forecasts dataframe, add the forecast to the corresponding column
    for (variable in c("income_ex", "direct_costs", "overheads", "total_headcount")) {
      forecast_column <- paste(variable, "var_forecast", sep = "_")
      
      # Initialize the forecast column if it doesn't exist
      if (!(forecast_column %in% names(data))) {
        data[[forecast_column]] <- NA
      }
      
      data[matching_rows, forecast_column] <- var_forecasts_df[[variable]][i]
    }
  }
  
  return(data)
}


