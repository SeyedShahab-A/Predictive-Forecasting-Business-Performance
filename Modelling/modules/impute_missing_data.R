#impute_missing_data.R

# Load necessary libraries
library(imputeTS)
library(dplyr)



# Function to impute missing values using na_kalman with auto.arima model
impute_missing_values <- function(data, variable_name, division = NULL) {
  # Filter data by division if provided
  if (!is.null(division)) {
    data <- data %>% filter(Division == !!division)
  }
  
  # Get unique cost centres
  cost_centres <- unique(data$Cost_Centre)
  
  # Initialize a list to store the imputed data for each cost centre
  imputed_data_list <- list()
  
  # Iterate over each cost centre
  for (cost_centre in cost_centres) {
    # Filter data for the current cost centre
    cost_centre_data <- data %>% filter(Cost_Centre == !!cost_centre)
    
    if (any(is.na(cost_centre_data[[variable_name]]))) {
      # Apply na_kalman to the specified variable
      cost_centre_data[[variable_name]] <- na_kalman(cost_centre_data[[variable_name]], model = "auto.arima")
    }
    
    # Add the imputed data to the list
    imputed_data_list[[cost_centre]] <- cost_centre_data
  }
  
  # Combine the imputed data for all cost centres
  imputed_data <- bind_rows(imputed_data_list)
  
  return(imputed_data)
}

?na_kalman
