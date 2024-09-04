#apply_models_to_all_vars4.R

library(forecast)
library(dplyr)
library(lubridate)

# Prepare xreg data function
prepare_xreg <- function(data, xreg_vars, start_index, end_index) {
  if (is.null(xreg_vars)) return(NULL)
  
  # Ensure the indices are valid
  start_index <- max(1, start_index)
  end_index <- min(nrow(data), end_index)
  
  # Extract the xreg variables
  xreg_data <- data[start_index:end_index, xreg_vars, drop = FALSE]
  
  return(as.matrix(xreg_data))
}

apply_models_to_all_vars <- function(data, date_column, variables, one_step = FALSE, model_t = NULL) {

  if (!is.character(variables)) {
    stop("Variables must be a character vector")
  }
  
  results <- list()
   
  for (var in variables) {
    # Check if the variable has missing values
    if (any(is.na(data[[var]]))) {
      next
    }
    
    if (var %in% names(data)) { 
      
      start_year <- year(min(data[[date_column]], na.rm = TRUE))
      start_month <- month(min(data[[date_column]], na.rm = TRUE))
      end_year <- year(max(data[[date_column]], na.rm = TRUE))
      end_month <- month(max(data[[date_column]], na.rm = TRUE))
      
      ts_data <- ts(data[[var]], frequency = 12, start = c(start_year, start_month), end = c(end_year, end_month))
      
      test_size <- 11
      split <- split_data(ts_data, test_size = test_size)
      train_data <- split$train
      test_data <- split$test
      
      start_index_train <- 1
      end_index_train <- length(train_data)
      start_index_test <- length(train_data) + 1
      end_index_test <- length(ts_data)
      
      #xreg_train <- prepare_xreg(data, xreg_vars, start_index_train, end_index_train)
      #xreg_test <- prepare_xreg(data, xreg_vars, start_index_test, end_index_test)
      
      if (is.null(model_t)) {
        # Apply all models
        if (one_step) {
          ets_result <- one_step_forecast_ets(train_data, test_data)
          arima_result <- one_step_forecast_arima(train_data, test_data)
          #bagged_ets_result <- one_step_forecast_bagged_ETS(train_data, test_data)
          #bagged_arima_result <- one_step_forecast_bagged_arima(train_data, test_data)

        } else {
          ets_result <- fit_and_validate_ets(train_data, test_data)
          arima_result <- fit_and_validate_arima(train_data, test_data)
          #bagged_ets_result <- fit_and_validate_bagged_ets(train_data, test_data)
          #bagged_arima_result <- fit_and_validate_bagged_arima(train_data, test_data)

        }
        results[[var]] <- list(
          ets = ets_result, 
          arima = arima_result, 
          #baggedets = bagged_ets_result,
          #baggedarima = bagged_arima_result,
          ts_data = ts_data, 
          test_data = test_data, 
          test_size = test_size
        )
      } else {
        # Apply the specified model
        if (model_t == "ets") {
          if (one_step) {
            model_result <- one_step_forecast_ets(train_data, test_data)
          } else {
            model_result <- fit_and_validate_ets(train_data, test_data)
          }
        } else if (model_t == "arima") {
          if (one_step) {
            model_result <- one_step_forecast_arima(train_data, test_data)
          } else {
            model_result <- fit_and_validate_arima(train_data, test_data)
          }
        } else if (model_t == "baggedets") {
          if (one_step) {
            model_result <- one_step_forecast_bagged_ets(train_data, test_data)
          } else {
            model_result <- fit_and_validate_bagged_ets(train_data, test_data)
          }
        } else if (model_t == "baggedarima") {
          if (one_step) {
            model_result <- one_step_forecast_bagged_arima(train_data, test_data)
          } else {
            model_result <- fit_and_validate_bagged_arima(train_data, test_data)
          }
        } else if (model_t == "nnetar") {
          if (one_step) {
            model_result <- one_step_forecast_nnetar(train_data, test_data)
          } else {
            model_result <- fit_and_validate_nnetar(train_data, test_data)
          }
          
        } else if (model_t == "grnn") {
          if (one_step) {
            model_result <- one_step_forecast_grnn(train_data, test_data)
          } else {
            model_result <- fit_and_validate_grnn(train_data, test_data)
          }
          
        } else {
          stop("Unsupported model type. Please use 'ets', 'arima', 'baggedets', 'baggedarima', or 'nnetar'.")
        }
        results[[var]] <- list(
          ts_data = ts_data, 
          test_data = test_data, 
          test_size = test_size
        )
        results[[var]][[model_t]] <- model_result
      }
    }
  }
  return(results)
}


missing_var_cost_centre <- function(data, variables){
  
  skipped_vars <- list()
  for (var in variables_to_include) {
    if (any(is.na(data[[var]]))) {
      skipped_cost_centres <- unique(data$Cost_Centre[is.na(data[[var]])])
      for (cost_centre in skipped_cost_centres) {
        skipped_vars <- c(skipped_vars, paste("Cost_Centre:", cost_centre, "- Variable:", var))
      }
    }
  }
  if (length(skipped_vars) > 0) {
    cat("Skipped due to missing values:\n", paste(skipped_vars, collapse = "\n"), "\n")
  }
}
