#calculate_mean_accuracy.R
library(dplyr)


calculate_mean_manager_accuracy <- function(manager_accuracy_df, variable) {

  filtered_df <- manager_accuracy_df %>% filter(Variable == variable)
  

  test_accuracy_matrix <- filtered_df %>%
    select(ME, RMSE, MAE, MPE, MAPE) %>%
    as.matrix()
  

  mean_test_accuracy <- colMeans(test_accuracy_matrix, na.rm = TRUE)
  

  mean_accuracy_matrix <- rbind(mean_test_accuracy)
  rownames(mean_accuracy_matrix) <- c("Test set")
  
  return(mean_accuracy_matrix)
}


calculate_mean_accuracy <- function(accuracy_df, variable) {

  variable_df <- accuracy_df %>% filter(Variable == variable)
  
  # Replace Inf and -Inf with NA in MPE and MAPE columns
  variable_df <- variable_df %>%
    mutate(MPE = ifelse(is.infinite(MPE) | MPE > 100 | MPE < -100, NA, MPE),
           MAPE = ifelse(is.infinite(MAPE) | MAPE > 100 | MAPE < -100, NA, MAPE))
  
  # Group by model and calculate mean accuracy metrics
  mean_accuracy_df <- variable_df %>%
    group_by(Model) %>%
    summarise(
      Variable = variable,
      ME = mean(ME, na.rm = TRUE),
      RMSE = mean(RMSE, na.rm = TRUE),
      MAE = mean(MAE, na.rm = TRUE),
      MPE = mean(MPE, na.rm = TRUE),
      MAPE = mean(MAPE, na.rm = TRUE),
      MASE = mean(MASE, na.rm = TRUE)
    ) %>%
    dplyr :: select(Model, Variable, ME, RMSE, MAE, MPE, MAPE, MASE)
  
  return(mean_accuracy_df)
}


