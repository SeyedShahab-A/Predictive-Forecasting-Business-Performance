#Compare.R

library(dplyr)


# Function to calculate the percentage of managers outperformed by each model for a given variable
calculate_performance <- function(data, variable) {

  df <- data %>% filter(Variable == variable)
  
  # Separate the manager and model rows
  manager_rmse <- df %>% filter(Model == "Manager") %>% select(Cost_Centre, RMSE)
  model_rmses <- df %>% filter(Model != "Manager") %>% select(Cost_Centre, Model, RMSE)
  
  # Merge model and manager RMSEs by Cost_Centre to ensure alignment
  merged_rmses <- merge(model_rmses, manager_rmse, by = "Cost_Centre", suffixes = c("_model", "_manager"))
  
  # Initialize a dataframe to store the results
  results <- data.frame(Model = character(),
                        Percentage = numeric(),
                        stringsAsFactors = FALSE)
  
  # Loop through each model and calculate the percentage of managers it outperformed
  for (model in unique(merged_rmses$Model)) {
    model_rmse <- merged_rmses %>% filter(Model == model) %>% select(RMSE_model) %>% pull()
    manager_rmse <- merged_rmses %>% filter(Model == model) %>% select(RMSE_manager) %>% pull()
    
    if (length(model_rmse) > 0 && length(manager_rmse) > 0) {
      outperformed_percentage <- mean(model_rmse < manager_rmse, na.rm = TRUE) * 100
      results <- rbind(results, data.frame(Model = model,
                                           Percentage = outperformed_percentage))
    }
  }
  
  # Rename the percentage column to the variable name
  colnames(results)[2] <- variable
  return(results)
}

# Function to calculate performance for all variables and bind results
calculate_performance_all <- function(data) {
  variables <- unique(data$Variable)
  results <- data.frame(Model = character(), stringsAsFactors = FALSE)
  
  for (variable in variables) {
    result <- calculate_performance(data, variable)
    if (nrow(results) == 0) {
      results <- result
    } else {
      results <- full_join(results, result, by = "Model")
    }
  }
  
  return(results)
}


