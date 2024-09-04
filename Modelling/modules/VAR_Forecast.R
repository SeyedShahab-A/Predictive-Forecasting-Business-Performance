library(vars)
library(forecast)

apply_var_model <- function(data, variables, n_forecast = 12, difference = FALSE, log_transform = FALSE) {
  train_data <- data[, variables, drop = FALSE]
  train <- train_data
  
  if (log_transform) {
    train <- log(train)
  }
  
  if (difference) {
    last_observed_values <- tail(train, 1)
    train <- lapply(train, diff, 1)
    train <- as.data.frame(train)
  }
  
  var_model <- VAR(train, lag.max = 6, ic = 'AIC', type = "both")
  #var_model_log <- VAR(train, p= 4, type = "both")
  
  var_forecast <- predict(var_model, n.ahead = n_forecast)
  
  forecasted_values_df <- data.frame(lapply(variables, function(var) {
    var_forecast$fcst[[var]][, "fcst"]
  }))
  
  colnames(forecasted_values_df) <- variables
  
  if (difference) {
    last_observed_values_df <- as.data.frame(last_observed_values, nrow = 1)
    combined_forecast <- rbind(last_observed_values_df, forecasted_values_df)
    reconstructed_forecast <- as.data.frame(sapply(1:ncol(forecasted_values_df), function(i) {
      cumsum(combined_forecast[, i])
    }))
    reconstructed_forecast <- reconstructed_forecast[-1, ]
    colnames(reconstructed_forecast) <- colnames(forecasted_values_df)
    forecasted_values_df <- reconstructed_forecast
  }
  
  if (log_transform) {
    forecasted_values_df <- exp(forecasted_values_df)
  }
  
  return(list(forecast = forecasted_values_df, model = var_model_log))
}

forecast_var_all <- function(dataset, variables, n_forecast = 12, difference = FALSE, log_transform = FALSE) {
  cost_centres <- unique(dataset$Cost_Centre)
  forecast_list <- list()
  accuracy_list <- list()
  model_list <- list()  
  
  for (cost_centre in cost_centres) {
    subset_data <- subset(dataset, Cost_Centre == cost_centre)
    train_data <- head(subset_data, -n_forecast)
    test_data <- tail(subset_data, n_forecast)
    
    var_result <- apply_var_model(train_data, variables, n_forecast, difference, log_transform)
    forecasts_df <- var_result$forecast
    var_model_log <- var_result$model
    
    forecasts_df$Date <- test_data$Date
    forecasts_df$Cost_Centre <- cost_centre
    forecasts_df$Division <- unique(subset_data$Division)
    
    for (var in variables) {
      accuracy_metrics <- accuracy(forecasts_df[[var]], test_data[[var]])
      
      accuracy_df <- data.frame(
        Cost_Centre = cost_centre,
        Division = unique(train_data$Division),
        Variable = var,
        Start_Date = min(test_data$Date),
        Test_Size = n_forecast,
        Model = "VAR",
        ME = accuracy_metrics[1, "ME"],
        RMSE = accuracy_metrics[1, "RMSE"],
        MAE = accuracy_metrics[1, "MAE"],
        MPE = accuracy_metrics[1, "MPE"],
        MAPE = accuracy_metrics[1, "MAPE"],
        MASE = NA  
      )
      
      accuracy_list[[length(accuracy_list) + 1]] <- accuracy_df
    }
    

    model_list[[cost_centre]] <- var_model
    
    forecast_list[[cost_centre]] <- forecasts_df
  }
  
  all_forecasts_df <- do.call(rbind, forecast_list)
  all_accuracy_df <- do.call(rbind, accuracy_list)
  
  return(list(
    forecasts = all_forecasts_df,
    accuracy = all_accuracy_df,
    models = model_list 
  ))
}


extract_var_model_info <- function(model_list) {

  info_list <- list()
  

  for (cost_centre in names(model_list)) {
    var_model <- model_list[[cost_centre]]
    
    # Number of lags
    num_lags <- var_model$p
    
    # Number of observations
    num_obs <- var_model$obs
    

    model_summary <- summary(var_model)
    
    variable_info <- list()
    
    # Extracting adjusted R-squared, F-statistic, and degrees of freedom for each variable
    for (var in names(model_summary$varresult)) {
      adj_r_squared <- model_summary$varresult[[var]]$adj.r.squared
      f_statistic <- model_summary$varresult[[var]]$fstatistic[1]
      numerator_df <- model_summary$varresult[[var]]$fstatistic[2]
      denominator_df <- model_summary$varresult[[var]]$fstatistic[3]
      
      variable_info[[var]] <- data.frame(
        Cost_Centre = cost_centre,
        Variable = var,
        Num_Lags = num_lags,
        Num_Obs = num_obs,
        Adj_R_Squared = adj_r_squared,
        F_Statistic = f_statistic,
        Numerator_DF = numerator_df,
        Denominator_DF = denominator_df
      )
    }
    

    info_list[[cost_centre]] <- do.call(rbind, variable_info)
  }
  

  all_info_df <- do.call(rbind, info_list)
  
  return(all_info_df)
} 
  
extract_var_roots_info <- function(model_list) {

    info_list <- list()
    
    for (cost_centre in names(model_list)) {
      var_model <- model_list[[cost_centre]]
      

      num_obs <- var_model$obs
      
      roots <- roots(var_model)
      
      roots_df <- data.frame(
        Cost_Centre = cost_centre,
        Num_Obs = num_obs,
        Roots = as.vector(roots) 
      )
      
      info_list[[cost_centre]] <- roots_df
    }
    
    # Combine all cost centre data frames into one
    all_info_df <- do.call(rbind, info_list)
    
    return(all_info_df)
}

extract_var_roots_list <- function(model_list) {
  # Initialize a list to store the roots for each cost centre
  roots_info <- list()
  
  # Loop through each cost centre's model
  for (cost_centre in names(model_list)) {
    var_model <- model_list[[cost_centre]]
    
    # Number of observations
    num_obs <- var_model$obs
    
    # Extract the roots of the VAR model
    roots <- roots(var_model)
    
    # Sort the roots in descending order
    sorted_roots <- sort(roots, decreasing = TRUE)
    
    # Store the number of observations and sorted roots in a list for this cost centre
    roots_info[[cost_centre]] <- list(
      Num_Obs = num_obs,
      Roots = sorted_roots
    )
  }
  
  return(roots_info)
}



check_var_stability_and_arch <- function(model_list) {

  stability_info <- list()
  
  for (cost_centre in names(model_list)) {
    var_model <- model_list[[cost_centre]]
    
    num_obs <- var_model$obs
    
    lag_order <- var_model$p
    
    roots <- roots(var_model)
    
    # Check if any root is equal to or greater than one
    has_root_one <- any(roots >= 1)
    
    stability <- ifelse(has_root_one, "Unstable", "Stable")
    
    # Perform ARCH test
    arch_test_result <- arch.test(var_model)
    p_value <- arch_test_result$arch.mul$p.value
    
    # Determine if there is an ARCH effect (p-value < 0.05 indicates an ARCH effect)
    has_arch_effect <- ifelse(p_value < 0.05, "Yes", "No")
    
    stability_info[[cost_centre]] <- data.frame(
      Cost_Centre = cost_centre,
      Num_Obs = num_obs,
      Lag_Order = lag_order,
      Stability = stability,
      Arch_Test_P_Value = p_value,
      Has_ARCH_Effect = has_arch_effect
    )
  }
  
  all_stability_df <- do.call(rbind, stability_info)
  
  return(all_stability_df)
}


calculate_f_stat_p_values <- function(info_df) {
  # Calculate the p-value for each row based on the F-statistic and degrees of freedom
  info_df$P_Value <- apply(info_df, 1, function(row) {
    f_statistic <- as.numeric(row["F_Statistic"])
    num_df <- as.numeric(row["Numerator_DF"])
    den_df <- as.numeric(row["Denominator_DF"])
    
    # Calculate the p-value using the F-distribution
    p_value <- pf(f_statistic, num_df, den_df, lower.tail = FALSE)
    return(p_value)
  })
  
  return(info_df)
}


add_serial_test_results <- function(stability_arch_df, model_list) {

  for (cost_centre in names(model_list)) {
    var_model <- model_list[[cost_centre]]
    
    # Perform the serial correlation test
    serial_test_result <- serial.test(var_model, lags.pt=8)
    p_value <- serial_test_result$serial$p.value
    
    # Determine if there is serial correlation (p-value < 0.05 indicates serial correlation)
    has_serial_correlation <- ifelse(p_value < 0.05, "Yes", "No")
    
    
    row_index <- which(stability_arch_df$Cost_Centre == cost_centre)
    

    stability_arch_df$Serial_Test_P_Value[row_index] <- p_value
    stability_arch_df$Has_Serial_Correlation[row_index] <- has_serial_correlation
  }
  
  return(stability_arch_df)
}