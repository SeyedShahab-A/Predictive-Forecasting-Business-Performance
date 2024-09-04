#extract_accuracy.R
# Function to extract test accuracy for all models and variables
extract_test_accuracy <- function(results) {
  accuracy_df <- data.frame()
  
  for (i in seq_along(results$model_results)) {
    cost_centre <- results$Cost_Centre[i]
    division <- results$Division[i]  # Access Division correctly
    
    for (var in names(results$model_results[[i]])) {
      if (var %in% c("ts_data", "test_data", "test_size")) next
      
      model_results <- results$model_results[[i]][[var]]
      
      start_date <- as.Date(paste(start(model_results$test_data)[1], start(model_results$test_data)[2], "01", sep = "-"))
      test_size <- model_results$test_size
      
      # Extract ARIMA accuracy
      if (!is.null(model_results$arima$accuracy)) {
        arima_accuracy <- model_results$arima$accuracy[2, ]
        accuracy_df <- rbind(accuracy_df, data.frame(
          Cost_Centre = cost_centre,
          Division = division,
          Variable = var,
          Start_Date = start_date,
          Test_Size = test_size,
          Model = "ARIMA",
          ME = arima_accuracy["ME"],
          RMSE = arima_accuracy["RMSE"],
          MAE = arima_accuracy["MAE"],
          MPE = arima_accuracy["MPE"],
          MAPE = arima_accuracy["MAPE"],
          MASE = arima_accuracy["MASE"]
        ))
      }
      
      # Extract ETS accuracy
      if (!is.null(model_results$ets$accuracy)) {
        ets_accuracy <- model_results$ets$accuracy[2, ]
        accuracy_df <- rbind(accuracy_df, data.frame(
          Cost_Centre = cost_centre,
          Division = division,
          Variable = var,
          Start_Date = start_date,
          Test_Size = test_size,
          Model = "ETS",
          ME = ets_accuracy["ME"],
          RMSE = ets_accuracy["RMSE"],
          MAE = ets_accuracy["MAE"],
          MPE = ets_accuracy["MPE"],
          MAPE = ets_accuracy["MAPE"],
          MASE = ets_accuracy["MASE"]
        ))
      }
      
      # Extract Bagged arima accuracy
      if (!is.null(model_results$baggedarima$accuracy)) {
        baggedarima_accuracy <- model_results$baggedarima$accuracy[2, ]
        accuracy_df <- rbind(accuracy_df, data.frame(
          Cost_Centre = cost_centre,
          Division = division,
          Variable = var,
          Start_Date = start_date,
          Test_Size = test_size,
          Model = "BaggedARIMA",
          ME = baggedarima_accuracy["ME"],
          RMSE = baggedarima_accuracy["RMSE"],
          MAE = baggedarima_accuracy["MAE"],
          MPE = baggedarima_accuracy["MPE"],
          MAPE = baggedarima_accuracy["MAPE"],
          MASE = baggedarima_accuracy["MASE"]
        ))
      }
      
      # Extract Bagged ets accuracy
      if (!is.null(model_results$baggedets$accuracy)) {
        baggedets_accuracy <- model_results$baggedets$accuracy[2, ]
        accuracy_df <- rbind(accuracy_df, data.frame(
          Cost_Centre = cost_centre,
          Division = division,
          Variable = var,
          Start_Date = start_date,
          Test_Size = test_size,
          Model = "BaggedETS",
          ME = baggedets_accuracy["ME"],
          RMSE = baggedets_accuracy["RMSE"],
          MAE = baggedets_accuracy["MAE"],
          MPE = baggedets_accuracy["MPE"],
          MAPE = baggedets_accuracy["MAPE"],
          MASE = baggedets_accuracy["MASE"]
        ))
      }
      
      # Extract nnetar accuracy
      if (!is.null(model_results$nnetar$accuracy)) {
        nnetar_accuracy <- model_results$nnetar$accuracy[2, ]
        accuracy_df <- rbind(accuracy_df, data.frame(
          Cost_Centre = cost_centre,
          Division = division,
          Variable = var,
          Start_Date = start_date,
          Test_Size = test_size,
          Model = "NNETAR",
          ME = nnetar_accuracy["ME"],
          RMSE = nnetar_accuracy["RMSE"],
          MAE = nnetar_accuracy["MAE"],
          MPE = nnetar_accuracy["MPE"],
          MAPE = nnetar_accuracy["MAPE"],
          MASE = nnetar_accuracy["MASE"]
          
        ))
      }
          
          # Extract grnn accuracy
          if (!is.null(model_results$grnn$accuracy)) {
            grnn_accuracy <- model_results$grnn$accuracy[1, ]
            accuracy_df <- rbind(accuracy_df, data.frame(
              Cost_Centre = cost_centre,
              Division = division,
              Variable = var,
              Start_Date = start_date,
              Test_Size = test_size,
              Model = "GRNN",
              ME = grnn_accuracy["ME"],
              RMSE = grnn_accuracy["RMSE"],
              MAE = grnn_accuracy["MAE"],
              MPE = grnn_accuracy["MPE"],
              MAPE = grnn_accuracy["MAPE"],
              MASE = grnn_accuracy["MASE"]
        ))
      }
    }
  }
  
  return(accuracy_df)
}



# Function to extract manager accuracy for all variables
extract_manager_accuracies <- function(data, variables, start_date = "2023-05-01",
                                       n = 12, one_period = TRUE) {
  accuracy_df <- data.frame()
  cost_centres <- unique(data$Cost_Centre)
  

  if (!inherits(data$Date, "Date")) {
    data$Date <- as.Date(data$Date)
  }
  
  for (cost_centre in cost_centres) {
    filtered_data <- data[data$Cost_Centre == cost_centre, ]
    division <- unique(filtered_data$Division)[1]
    # Find the start index based on the start_date
    start_index <- which(filtered_data$Date == start_date)
    if (length(start_index) == 0) {
      stop("start_date not found in the data for cost centre: ", cost_centre)
    }
    
    while (start_index > 0) {
      for (var in variables) {
        print(paste("Cost Centre:", cost_centre))
        print(paste("Variable:", var))
        print(paste("Start Index:", start_index))
        
        manager_accuracy <- calculate_manager_accuracy2(filtered_data, variable = var, start_index = start_index, n = min(n, length(start_index:(start_index + n - 1))))
        
        if (!is.null(manager_accuracy)) {
          print(paste("ME:", manager_accuracy[ ,"ME"]))
          print(paste("RMSE:", manager_accuracy[ ,"RMSE"]))
          print(paste("MAE:", manager_accuracy[ ,"MAE"]))
          print(paste("MPE:", manager_accuracy[ ,"MPE"]))
          print(paste("MAPE:", manager_accuracy[ ,"MAPE"]))
          print(paste("MASE:", NA))
          
          accuracy_df <- rbind(accuracy_df, data.frame(
            Cost_Centre = cost_centre,
            Division = division,
            Variable = var,
            Start_Date = filtered_data$Date[start_index],
            Test_Size = min(n, length(start_index:(start_index + n - 1))),
            Model = "Manager",
            ME = manager_accuracy[ ,"ME"],
            RMSE = manager_accuracy[ ,"RMSE"],
            MAE = manager_accuracy[ ,"MAE"],
            MPE = manager_accuracy[ ,"MPE"],
            MAPE = manager_accuracy[ ,"MAPE"],
            MASE = NA  
          ))
        }
      }
      
      # Move to the previous 12-month period
      if (one_period) {
        start_index <- 0  # Exit the loop after the first period
      } else {
        start_index <- start_index - n
      }
    }
  }
  
  return(accuracy_df)
}



# Function to combine multiple accuracy dataframes
combine_accuracy_dfs <- function(..., fill = NA) {
  dfs <- list(...)
  combined_df <- do.call(rbind, dfs)
  combined_df[is.na(combined_df)] <- fill
  
  combined_df <- combined_df %>%
    arrange(Cost_Centre, Variable, Model)
  
  
  return(combined_df)
}