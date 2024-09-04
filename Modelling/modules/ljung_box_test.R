library(forecast)
library(tseries)
library(dplyr)

ljung_box_test <- function(results, variable, model_type) {
  cost_centres <- results$Cost_Centre
  ljung_box_results <- data.frame(
    Cost_Centre = character(),
    X_squared = numeric(),
    Parameter = integer(),
    P_Value = numeric(),
    Method = character(),
    Data_Name = character(),
    stringsAsFactors = FALSE
  )
  
  residuals_list <- list()
  
  for (i in seq_along(results$model_results)) {
    cost_centre <- cost_centres[i]
    forecasts <- results$model_results[[i]][[variable]]
    
    residuals <- forecasts[[model_type]]$model$residuals
    
    ljung_box_test <- Box.test(residuals, type = "Ljung-Box")
    
    # Extract relevant components from the Ljung-Box test
    ljung_box_info <- data.frame(
      Cost_Centre = cost_centre,
      X_squared = as.numeric(ljung_box_test$statistic),
      Parameter = as.integer(ljung_box_test$parameter),
      P_Value = as.numeric(ljung_box_test$p.value),
      Method = as.character(ljung_box_test$method),
      Data_Name = as.character(ljung_box_test$data.name),
      stringsAsFactors = FALSE
    )
    
    ljung_box_results <- rbind(ljung_box_results, ljung_box_info)
    
    residuals_list[[cost_centre]] <- residuals
  }
  
  return(list(ljung_box_results = ljung_box_results, residuals_list = residuals_list))
}

