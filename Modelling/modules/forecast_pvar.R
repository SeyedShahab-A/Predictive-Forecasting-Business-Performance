library(forecast)


forecast_pvar_fd <- function(pvar_model, subset_data, variables, n_forecast = 12, back_transform = FALSE) {
  # Extract the relevant numeric columns for forecasting based on the given variables
  data_for_forecasting <- subset_data[, variables, drop = FALSE]
  
  # Calculate first differences for the training data
  diff_data <- lapply(data_for_forecasting, diff, 1)
  diff_data <- as.data.frame(diff_data) 
  
  coefs <- coef(pvar_model)
  
  # Identify the number of lags based on the coefficient names
  lag_vars <- grep("^fd_lag[0-9]+_", colnames(coefs), value = TRUE)
  max_lag <- max(as.numeric(gsub("fd_lag([0-9]+)_.*", "\\1", lag_vars)))
  
  # Initialize the level forecasts with the last observed levels
  level_forecasts <- matrix(NA, nrow = n_forecast, ncol = length(variables))
  colnames(level_forecasts) <- variables
  
  # Initialize a history to store past values for lags (using first differences)
  history <- tail(diff_data, max_lag)
  
  # Last known levels (before the forecast period)
  last_known_levels <- tail(data_for_forecasting, 1)
  

  for (i in 1:n_forecast) {
    new_fd_forecast <- setNames(numeric(length(variables)), variables)
    new_level_forecast <- setNames(numeric(length(variables)), variables)
    
    for (var in variables) {
      # Start with the constant term
      fd_forecast_value <- coefs[paste0("fd_", var), "const"]
      
      for (lag_var in lag_vars) {
        # Extract the lag number and the variable name
        lag_num <- as.numeric(gsub("fd_lag([0-9]+)_.*", "\\1", lag_var))
        orig_var_name <- gsub("fd_lag[0-9]+_", "", lag_var)
        
        # Get the corresponding lagged value from history (for first differences)
        lag_value <- history[nrow(history) - lag_num + 1, orig_var_name]
        
        # Add the contribution of this lagged variable to the forecast
        fd_forecast_value <- fd_forecast_value + coefs[paste0("fd_", var), lag_var] * lag_value
      }
      
      # Store the forecasted first difference for this variable
      new_fd_forecast[var] <- fd_forecast_value
      
      # Convert the first difference forecast to a level forecast
      if (i == 1) {
        # For the first forecast, add to the last observed level
        new_level_forecast[var] <- last_known_levels[[var]] + fd_forecast_value
      } else {
        # For subsequent forecasts, add to the previous level forecast
        new_level_forecast[var] <- level_forecasts[i - 1, var] + fd_forecast_value
      }
    }
    
    # Update the history by appending the new forecasted first differences
    history <- rbind(history[-1, , drop = FALSE], as.numeric(new_fd_forecast))
    
    level_forecasts[i, ] <- new_level_forecast
  }
  
  # Back-transform the level forecasts if data was log-transformed
  if (back_transform) {
    level_forecasts <- exp(level_forecasts)
  }
  
  return(level_forecasts)
}



forecast_all_cost_centres <- function(pvar_model, train_data, test_data, variables, back_transform = FALSE) {

  cost_centres <- unique(train_data$Cost_Centre)

  forecast_list <- list()
  accuracy_list <- list()
  

  for (cost_centre in cost_centres) {

    train_subset <- subset(train_data, Cost_Centre == cost_centre)
    test_subset <- subset(test_data, Cost_Centre == cost_centre)
    

    n_forecast <- nrow(test_subset)
    
    forecasts <- forecast_pvar_fd(pvar_model, train_subset, variables, n_forecast, back_transform)
    

    forecasts_df <- as.data.frame(forecasts)
    forecasts_df$Cost_Centre <- cost_centre
    forecasts_df$Date <- test_subset$Date  
    
    # Calculate accuracy metrics for each variable
    for (var in variables) {
      accuracy_metrics <- accuracy(forecasts_df[[var]], test_subset[[var]])
      
      accuracy_df <- data.frame(
        Cost_Centre = cost_centre,
        Division = unique(train_subset$Division),
        Variable = var,
        Start_Date = min(test_subset$Date),
        Test_Size = n_forecast,
        Model = "PVAR",  
        ME = accuracy_metrics[1, "ME"],
        RMSE = accuracy_metrics[1, "RMSE"],
        MAE = accuracy_metrics[1, "MAE"],
        MPE = accuracy_metrics[1, "MPE"],
        MAPE = accuracy_metrics[1, "MAPE"],
        MASE = NA  
      )
      
      accuracy_list[[length(accuracy_list) + 1]] <- accuracy_df
    }
    

    forecast_list[[cost_centre]] <- forecasts_df
  }
  
  all_forecasts_df <- do.call(rbind, forecast_list)
  
  all_accuracy_df <- do.call(rbind, accuracy_list)
  
  return(list(
    forecasts = all_forecasts_df,
    accuracy = all_accuracy_df
  ))
}
