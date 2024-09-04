# main_analysis.R


library(readr)
library(dplyr)
library(zoo)
library(imputeTS)

# Source the external scripts
source('modules/data_splitting.R')
source('modules/exponential_smoothing.R')
source('modules/arima_model.R')
#source('modules/apply_models_to_all_vars.R')
#source('modules/apply_models_to_all_vars2.R')
source('modules/apply_models_to_all_vars3.R')
source('modules/apply_models_to_all_vars4.R')
source('modules/calculate_mean_accuracy.R')
source('modules/compare.R')
source('modules/extract_accuracy.R')
source('modules/add_forecasts_to_data.R')
source('modules/ljung_box_test.R')
source('modules/plot_functions.R')
source('modules/Plot_function2.R')
source('modules/Plot_functions1.R')
source('modules/Plot_functions3.R')
source('modules/Plot_functions4.R')
source('modules/baggedModels.R')
source('modules/nnetar_model.R')
source('modules/manager_accuracy.R')
source('modules/impute_missing_data.R')
source('modules/grnn.R')


# Load the combined CSV file
data_h <- read_csv("Combined files/hist_df.csv")

# Convert the 'Date' column to Date type
data_h$Date <- as.Date(data_h$Date)



duplicates <- duplicated(data_h)

# Display duplicate rows
duplicate_rows <- data_h[duplicates, ]
print(duplicate_rows, n=35)

duplicate_rows$Date

data_h <- data_h[!duplicated(data_h), ]

sum(is.na(data_h$total_headcount))

data_h['division'] == 'Project management'


data_h[data_h['Division'] == 'Project management', ] <- impute_missing_values(data_h[data_h['Division'] == 'Project management', ], 'total_headcount', division = 'Project management')

sum(duplicated(data_h))

#############################################################################################
# Example variables to include
variables_to_include <- c("income_ex", "direct_costs", 'overheads', 'total_headcount')

results_betsh <- data_h %>%
  group_by(Cost_Centre, Division) %>%
  summarize(
    model_results = list(apply_models_to_all_vars(cur_data(), "Date", variables_to_include, model_t = 'baggedets'))
  )


results_barimah <- data_h %>%
  group_by(Cost_Centre, Division) %>%
  summarize(
    model_results = list(apply_models_to_all_vars(cur_data(), "Date", variables_to_include, model_t = 'baggedarima'))
  )

results_ea <- data_h %>%
  group_by(Cost_Centre, Division) %>%
  summarize(
    model_results = list(apply_models_to_all_vars(cur_data(), "Date", variables_to_include))
  )

results_betsh$model_results[[10]]$income_ex$baggedets$

  
# Auto p without transform size 3
results_nnetar_h <- data_h %>%
  group_by(Cost_Centre, Division) %>%
  summarize(
    model_results = list(apply_models_to_all_vars(cur_data(), "Date", variables_to_include, model_t = 'nnetar'))
  )


#auto p transform
results_nnetar_h_tr <- data_h %>%
  group_by(Cost_Centre, Division) %>%
  summarize(
    model_results = list(apply_models_to_all_vars(cur_data(), "Date", variables_to_include, model_t = 'nnetar'))
  )

#p=3 transform
results_nnetar_h_p3 <- data_h %>%
  group_by(Cost_Centre, Division) %>%
  summarize(
    model_results = list(apply_models_to_all_vars(cur_data(), "Date", variables_to_include, model_t = 'nnetar'))
  )


results_nnetar_h_p9 <- data_h %>%
  group_by(Cost_Centre, Division) %>%
  summarize(
    model_results = list(apply_models_to_all_vars(cur_data(), "Date", variables_to_include, model_t = 'nnetar'))
  )


results_grnn <- data_h %>%
  group_by(Cost_Centre, Division) %>%
  summarize(
    model_results = list(apply_models_to_all_vars(cur_data(), "Date", variables_to_include, model_t = 'grnn'))
  )

# lag 1:5
results_grnn_lag5 <- data_h %>%
  group_by(Cost_Centre, Division) %>%
  summarize(
    model_results = list(apply_models_to_all_vars(cur_data(), "Date", variables_to_include, model_t = 'grnn'))
  )


?auto.arima
results_grnn$model_results[[40]]$direct_costs$grnn$accuracy

# Extract test accuracy for all models and variables
accuracy_df_ea <- extract_test_accuracy(results_ea)
accuracy_df_barimah <- extract_test_accuracy(results_barimah)
accuracy_df_betsh <- extract_test_accuracy(results_betsh)
accuracy_df_nnetar_h <- extract_test_accuracy(results_nnetar_h)

accuracy_df_nnetar_h_tr <- extract_test_accuracy(results_nnetar_h_tr)
accuracy_df_nnetar_h_p3_tr <- extract_test_accuracy(results_nnetar_h_p3)
accuracy_df_grnn <- extract_test_accuracy(results_grnn)
accuracy_df_grnn_l5 <- extract_test_accuracy(results_grnn_lag5)


grnn_accuracy

combined_accuracy_df_nnetar <- combine_accuracy_dfs(accuracy_df_nnetar_h_p9_tr, accuracy_df_manager_h)

combined_accuracy_df_h <- combine_accuracy_dfs(accuracy_df_ea,accuracy_df_barimah,
                                             accuracy_df_betsh, accuracy_df_manager_h, accuracy_df_var_all)
length(unique(combined_accuracy_df_h$Cost_Centre))

s<- combined_accuracy_df_h[!combined_accuracy_df_h['Division']== 'Amcl', ]

s<- s[!s['Division'] == 'Advisory', ]

mean_nnetar <- calculate_mean_accuracy(combined_accuracy_df_nnetar, 'income_ex')

results_grnn$

accuracy_df_manager_h <- extract_manager_accuracies(data_h, variables_to_include, 
                                                  start_date = "2023-05-01", n = 12, one_period = TRUE)

accuracy_df_manager_all_periods <- extract_manager_accuracies(data, variables_to_include, 
                                                              start_date = "2023-05-01", n = 12, one_period = FALSE)
acc_h <- combined_accuracy_df_h[!combined_accuracy_df_h$Cost_Centre == 'INF_MOD',]

th_mean <-calculate_mean_accuracy(acc_h, 'total_headcount')


calculate_mean_accuracy(acc_h, 'overheads')


calculate_performance_all(combined_accuracy_df_h)

performance_df <- calculate_performance_all(acc_with_nnetar)
print(performance_df)

# Specify the file name
file_name <- "Compare tables/th_mean.csv"

# Save the dataframe to a CSV file
write.csv(th_mean, file_name, row.names = FALSE)

# Print a message indicating the file was saved successfully
cat("Dataframe saved to", file_name, "successfully.\n")


data_with_f <- add_forecasts_to_data(data1, results_arima, 'total_headcount', 'arima')

for (i in seq_along(results_arima$model_results)) {
  if (results_arima$Cost_Centre[i] == "CM_NorthEast") {
    cost_centre <- results_arima$Cost_Centre[i]
    model_result <- results_arima$model_results[[i]]$income_ex$arima$model
    forecast_result <- results_arima$model_results[[i]]$income_ex$arima$forecast
    
    cat("\n")  # Correct way to print a newline in R
    cat(paste("Index:", i, "\n"))
    cat(paste("Cost Centre:", cost_centre, "\n"))
    print(summary(model_result))
  }
}

results_arima$model_results[[1]]
data_with_f <- add_forecasts_to_data(data_with_f, results_heacount, 'total_headcount', 'ets')

#############################################################################################
# Extract and print model parameters and accuracy metrics for the selected division
for (i in seq_along(results_arima$model_results)) {
  if (results_arima$Division[i] == "Project management") {
    cost_centre <- results_arima$Cost_Centre[i]
    model_result <- results_arima$model_results[[i]]$income_ex$arima$model
    forecast_result <- results_arima$model_results[[i]]$income_ex$arima$forecast
    
    cat("\n")  # Correct way to print a newline in R
    cat(paste("Index:", i, "\n"))
    cat(paste("Cost Centre:", cost_centre, "\n"))
    print(summary(model_result))
  }
}

ljung_box_results <- ljung_box_test(results_arima, "income_ex", 'arima')

print(ljung_box_results$ljung_box_results)

results_arima$model_results[[38]]$income_ex$arima$model

################################################################################
#ETS models

for (i in seq_along(results_ets$model_results)) {
  if (results_ets$Division[i] == "Project management") {
    cost_centre <- results_ets$Cost_Centre[i]
    model_result <- results_ets$model_results[[i]]$income_ex$ets$model
    
    # Extract model details
    model_name <- model_result$method
    model_parameters <- model_result$par
    aic <- model_result$aic
    
    # Print the desired information
    cat("\n")  # Print a newline
    cat(paste("Index:", i, "\n"))
    cat(paste("Cost Centre:", cost_centre, "\n"))
    cat(paste("Chosen ETS Model:", model_name, "\n"))
    cat(paste("Model Parameters:\n"))
    print(model_parameters)
    cat(paste("AIC of the Chosen Model:", aic, "\n"))
  }
}


model_type <- "ets"  # or "ets"
ljung_box_results <- ljung_box_test(results_ets, "income_ex", model_type)
print(ljung_box_results$ljung_box_results)


results_PM$model_results[[1]]$secured$ets$model$par

plot_forecasts_all(results_ets, "ets", "Project management", "direct_costs")
###################################################################################
# Direct_cost - project management

selected_division <- "Project management"
PM_data <- data %>% filter(Division == selected_division)

# Apply ARIMA models to the filtered data
results_PM_cost <- PM_data %>%
  group_by(Cost_Centre, Divison) %>%
  summarize(
    model_results = list(apply_models_to_all_vars(cur_data(), "Date", "direct_costs"))
  )



model_type <- 'arima'
plot_forecasts_all(results_bets, "baggedets", "Project management", "direct_costs")

results_bets$model_results[[10]]$income_ex$baggedets$forecast$lower



###################################################################################
#total_headcount analysis


variables <- c("total_headcount")

results_heacount <- data1 %>%
  group_by(Cost_Centre, Division) %>%
  summarize(
    model_results = list(apply_models_to_all_vars(cur_data(), "Date", variables))
  )


results_heacount$model_results[[2]]$total_headcount$baggedets$forecast
accuracy_df_headcount <- extract_test_accuracy(results_heacount)

accuracy_df_manager2 <- extract_manager_accuracies(data1, variables, 
                                                  start_date = "2023-05-01", n = 12, one_period = TRUE)



acc_df_headcounts<- combine_accuracy_dfs(accuracy_df_headcount,accuracy_df_manager2)


combined_accuracy_df_with_heacount<- combine_accuracy_dfs(combined_accuracy_df,accuracy_df_headcount)



calculate_performance_all(acc_df_headcounts)
headcount_mean_acc <- calculate_mean_accuracy(acc_df_headcounts, 'total_headcount')

file_name <- "combined_accuracy_df_all.csv"

# Save the dataframe to a CSV file
write.csv(combined_accuracy_df_all, file_name, row.names = FALSE)

# Print a message indicating the file was saved successfully
cat("Dataframe saved to", file_name, "successfully.\n")


####################################################################################


##################################################################################
#Direct_Costs ETS


for (i in seq_along(results_ets$model_results)) {
  if (results_ets$Division[i] == "Project management") {
    cost_centre <- results_ets$Cost_Centre[i]
    model_result <- results_ets$model_results[[i]]$direct_costs$ets$model
    
    # Extract model details
    model_name <- model_result$method
    model_parameters <- model_result$par
    aic <- model_result$aic
    
    # Print the desired information
    cat("\n")  # Print a newline
    cat(paste("Index:", i, "\n"))
    cat(paste("Cost Centre:", cost_centre, "\n"))
    cat(paste("Chosen ETS Model:", model_name, "\n"))
    cat(paste("Model Parameters:\n"))
    print(model_parameters)
    cat(paste("AIC of the Chosen Model:", aic, "\n"))
  }
}


model_type <- "ets"  # or "ets"
ljung_box_results <- ljung_box_test(results_ets, "direct_costs", model_type)
print(ljung_box_results$ljung_box_results)


results_PM$model_results[[1]]$secured$ets$model$par

plot_forecasts_all(results_ets, "ets", "Project management", "income_ex")


####################################################################################


#Direct_Costs ARIMA

for (i in seq_along(results_arima$model_results)) {
  if (results_arima$Division[i] == "Project management") {
    cost_centre <- results_arima$Cost_Centre[i]
    model_result <- results_arima$model_results[[i]]$direct_costs$arima$model
    forecast_result <- results_arima$model_results[[i]]$direct_costs$arima$forecast
    
    cat("\n")  # Correct way to print a newline in R
    cat(paste("Index:", i, "\n"))
    cat(paste("Cost Centre:", cost_centre, "\n"))
    print(summary(model_result))
  }
}

ljung_box_results <- ljung_box_test(results_arima, "direct_costs", 'arima')

print(ljung_box_results$ljung_box_results)

model_type <- "arima"  # or "ets"
ljung_box_results <- ljung_box_test(results_arima, "direct_costs", model_type)
print(ljung_box_results$ljung_box_results)


sd<- results_arima$model_results[[2]]$income_ex$arima$forecast

sd$mean <- window(sd$mean, end = c(2024, 3))
sd$lower <- window(sd$lower, end = c(2024, 3))
sd$upper <- window(sd$upper, end = c(2024, 3))

# If you want to update the x series (original time series) as well
sd$x <- window(sd$x, end = c(2024, 3))

sd$lower


library(tsfgrnn)

??tsfgrnn
