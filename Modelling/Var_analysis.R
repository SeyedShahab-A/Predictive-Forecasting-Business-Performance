

library(readr)
library(dplyr)
library(zoo)
library(imputeTS)
library(tibble)
library(vars)
library(tseries)
library(lubridate)
library(forecast)
library(stats)

# Source the external scripts
source('modules/data_splitting.R')
source('modules/calculate_mean_accuracy.R')
source('modules/compare.R')
source('modules/extract_accuracy.R')
source('modules/add_forecasts_to_data.R')
source('modules/plot_functions.R')
source('modules/Plot_function2.R')
source('modules/Plot_functions1.R')
source('modules/Plot_functions3.R')
source('modules/Plot_functions4.R')
source('modules/manager_accuracy.R')
source('modules/impute_missing_data.R')
source('modules/VAR_Forecast.R')


data_h1 <- read_csv("Combined files/hist_df.csv")

variables_to_include <- c("income_ex", "direct_costs", "overheads", 'total_headcount')

select_df <- data_h1[,  c("income_ex", "direct_costs", "overheads", 
                          'total_headcount', "Cost_Centre","Division", "Date")]

negative_income_centres <- unique(select_df$Cost_Centre[select_df$income_ex < 0])

negative_income_centres

excluded_cost_centres <- c("AMCL_AMA", "AMCL_Group", "AMCL_Vertex", "INF_MOD" )



select_df <- select_df[!select_df$Cost_Centre %in% excluded_cost_centres, ]

length(unique(select_df$Cost_Centre))


select_df[select_df <0] <- NA


df_ok <- impute_missing_values(df_ok, 'total_headcount')

df_ok[3106, 'direct_costs']

df_ok[3106,'direct_costs'] <- NA

df_ok1[df_ok1$Cost_Centre == 'ADV_Pro', 'total_headcount']

df_ok1[df_ok1$total_headcount ==0, 'total_headcount' ] <- NA
duplicated
sum(is.na(df_ok))

df_ok <- df_ok[!duplicated(df_ok), ]
duplicates <- duplicated(df_ok)



# Display duplicate rows
duplicate_rows <- df_ok[duplicates, ]
print(duplicate_rows, n=35)

df_ok1 <- impute_missing_values(df_ok1, 'total_headcount')

sum(df_ok1 <0)

df_ok1 <- df_ok[!df_ok$Date == '2024-04-01', ]

###################################################################################################

variables <- c("income_ex", "direct_costs", "overheads")

var_results <- forecast_var_all(df_hok1, variables,
                                n_forecast = 11, difference = FALSE,
                                log_transform = FALSE)

var_results_diff <- forecast_var_all(df_ok1, variables,
                                n_forecast = 11, difference = TRUE,
                                log_transform = FALSE)

var_results_log <- forecast_var_all(df_ok, variables,
                                     n_forecast = 12, difference = FALSE,
                                     log_transform = TRUE)


var_results_log_diff <- forecast_var_all(df_ok1, variables,
                                    n_forecast = 11, difference = TRUE,
                                    log_transform = TRUE)
serial.test(var_model)

accuracy_df_var<- var_results$accuracy
accuracy_df_var_diff<- var_results_diff$accuracy
accuracy_df_var_log<- var_results_log$accuracy
accuracy_df_var_log_diff<- var_results_log_diff$accuracy



calculate_mean_accuracy(accuracy_df_var, 'direct_costs')
calculate_mean_accuracy(accuracy_df_var_log, 'overheads')
calculate_mean_accuracy(accuracy_df_var_diff, 'income_ex')
calculate_mean_accuracy(accuracy_df_var_log_diff, 'direct_costs')




var_forecasts <- var_results$forecasts

var_models <- var_results$models

hitech_model <- var_results_all$models$PM_Hitech

summary(hitech_model)





# Specify the file name
file_name <- "flok.csv"

# Save the dataframe to a CSV file
write.csv(flok, file_name, row.names = FALSE)

# Print a message indicating the file was saved successfully
cat("Dataframe saved to", file_name, "successfully.\n")
################################################################################

variables_to_include <- 



var_results_all <- forecast_var_all(df_hok1, variables_to_include,
                                  n_forecast = 11, difference = FALSE,
                                  log_transform = FALSE)

var_results_diff_all <- forecast_var_all(df_ok1, variables_to_include,
                                     n_forecast = 11, difference = TRUE,
                                     log_transform = FALSE)

var_results_log_all <- forecast_var_all(df_hok1, variables_to_include,
                                    n_forecast = 11, difference = FALSE,
                                    log_transform = TRUE)


var_results_log_diff_all <- forecast_var_all(df_ok1, variables_to_include,
                                         n_forecast = 11, difference = TRUE,
                                         log_transform = TRUE)



accuracy_df_var_all<- var_results_all$accuracy
accuracy_df_var_diff_all<- var_results_diff_all$accuracy
accuracy_df_var_log_all<- var_results_log_all$accuracy
accuracy_df_var_log_diff_all<- var_results_log_diff_all$accuracy



calculate_mean_accuracy(accuracy_df_var_all, 'direct_costs')
calculate_mean_accuracy(accuracy_df_var_log_all, 'direct_costs')
calculate_mean_accuracy(accuracy_df_var_diff_all, 'income_ex')
calculate_mean_accuracy(accuracy_df_var_log_diff_all, 'overheads')




var_forecasts_all <- var_results_all$forecasts

var_models_all <- var_results_all$models

hitech_model <- var_results_all$models$PM_Hitech

summary(hitech_model)


var_forecasts_all_log <- var_results_log_all$forecasts

var_models_all_log <- var_results_log_all$models


var_results_4 <- forecast_var_all(df_ok1, variables_to_include,
                                    n_forecast = 11, difference = FALSE,
                                    log_transform = FALSE)
accuracy_df_var_4<- var_results_6$accuracy

calculate_mean_accuracy(accuracy_df_var_4, 'total_headcount')
var_forecasts_4 <- var_results_4$forecasts

var_models_4 <- var_results_4$models




###################################################################################


var_results_1 <- forecast_var_all(df_1, variables_to_include,
                                n_forecast = 11, difference = FALSE,
                                log_transform = FALSE)

var_results_diff_1 <- forecast_var_all(df_1, variables_to_include,
                                     n_forecast = 12, difference = TRUE,
                                     log_transform = FALSE)

var_results_log_1 <- forecast_var_all(df_1, variables_to_include,
                                    n_forecast = 11, difference = FALSE,
                                    log_transform = TRUE)


var_results_log_diff_1 <- forecast_var_all(df_1, variables_to_include,
                                         n_forecast = 12, difference = TRUE,
                                         log_transform = TRUE)



accuracy_df_var_1<- var_results_1$accuracy
accuracy_df_var_diff1<- var_results_diff_1$accuracy
accuracy_df_var_log1<- var_results_log_1$accuracy
accuracy_df_var_log_diff<- var_results_log_diff_1$accuracy

fvar <- var_results_1$forecasts

calculate_mean_accuracy(accuracy_df_var_1, 'direct_costs')
calculate_mean_accuracy(accuracy_df_var_log1, 'direct_costs')
calculate_mean_accuracy(accuracy_df_var_diff1, 'income_ex')
calculate_mean_accuracy(accuracy_df_var_log_diff, 'direct_costs')





################################################################################
#Var for all Cost Centres

variables_to_include <- c("income_ex", "direct_costs", "overheads", 'total_headcount')

select_df <- data_l[,  c("income_ex", "direct_costs", "overheads", 
                          'total_headcount', "Cost_Centre","Division", "Date")]

negative_income_centres <- unique(select_df$Cost_Centre[select_df$income_ex < 0])

negative_income_centres

excluded_cost_centres <- c("AMCL_AMA", "AMCL_Group", "AMCL_Vertex", "INF_MOD", "CM_Co",  "INF_Co" )



sum(is.na(select_df))
select_df <- select_df[!select_df$Cost_Centre %in% excluded_cost_centres, ]

length(unique(select_df$Cost_Centre))

select_df[select_df <0]

select_df[select_df <0] <- NA


df_lok <- impute_missing_values(df_lok, 'total_headcount')

sum(is.na(df_lok))

df_lok[3778, 'direct_costs']

df_lok[3778,'direct_costs'] <- NA

df_lok[1032, 'overheads']

df_lok[1032,'overheads'] <- NA

df_lok[2611, 'overheads']

df_lok[2611,'overheads'] <- NA

df_lok <- impute_missing_values(df_lok, 'direct_costs')

df_lok[df_lok$Cost_Centre == 'ADV_Pro' & df_lok$Date == '2023-04-01',]$total_headcount <- NA

df_lok <- impute_missing_values(df_lok, 'total_headcount')

sum(is.na(df_lok))


duplicates <- duplicated(df_lok)
duplicate_rows <- df_lok[duplicates, ]
print(duplicate_rows, n=35)

df_lok <- df_lok[!duplicated(df_lok), ]



df_lok1 <- df_lok[!df_lok$Date == '2024-04-01', ]



###############################################################################


var_results_lok <- forecast_var_all(df_lok1, variables_to_include,
                                  n_forecast = 11, difference = FALSE,
                                  log_transform = FALSE)

var_results_diff_1 <- forecast_var_all(df_lok1, variables_to_include,
                                       n_forecast = 11, difference = TRUE,
                                       log_transform = FALSE)

var_results_log_lok <- forecast_var_all(df_lok1, variables_to_include,
                                      n_forecast = 11, difference = FALSE,
                                      log_transform = TRUE)


var_results_log_diff_1 <- forecast_var_all(df_1, variables_to_include,
                                           n_forecast = 12, difference = TRUE,
                                           log_transform = TRUE)
length(unique(df_lok1$Cost_Centre))
flok <- var_results_lok$forecasts

var_models <- var_results_lok$models

proc_model<- var_models$ADV_Proc

s<- summary(proc_model$varresult$direct_costs)
var_model$varresult$income_ex$assign

X<- summary(var_model)
X$varresult$income_ex$fstatistic[3]

accuracy_df_var_lok<- var_results_lok$accuracy
accuracy_df_var_diff1<- var_results_diff_1$accuracy
accuracy_df_var_loglok<- var_results_log_lok$accuracy
accuracy_df_var_log_diff<- var_results_log_diff_1$accuracy



calculate_mean_accuracy(accuracy_df_var_lok, 'total_headcount')
calculate_mean_accuracy(accuracy_df_var_loglok, 'total_headcount')
calculate_mean_accuracy(accuracy_df_var_diff1, 'income_ex')
calculate_mean_accuracy(accuracy_df_var_log_diff, 'direct_costs')




################################################################################

sample_cost_centre <- "PM_Hitech"  # Replace with an actual Cost_Centre value
sample_data <- df_ok1 %>% filter(Cost_Centre == sample_cost_centre)



unique(df_ok[df_ok$Division == 'Project management',]$Cost_Centre)

variables_to_include <- c("income_ex", "direct_costs", 'overheads')

# Split data for each variable
test_size <- 11

# Split the data
train_data <- sample_data[1:(nrow(sample_data) - test_size), ]
test_data <- sample_data[(nrow(sample_data) - test_size + 1):nrow(sample_data), ]

train_ts <- ts(train_data[, variables_to_include], start = c(year(min(train_data$Date))
                                                             , month(min(train_data$Date))), frequency = 12)
test_ts <- ts(test_data[, variables_to_include], start = c(year(min(test_data$Date))
                                                           , month(min(test_data$Date))), frequency = 12)


train_data <- head(sample_data[, variables_to_include], -11)
test_data <- tail(sample_data[, variables_to_include], 11)


#Stationarity Test
adf_test_results <- lapply(train_ts, adf.test, alternative = "stationary")
print(adf_test_results)

train_ts
################################################################################
# Fit VAR model
var_modell5 <- VAR(train_ts, p=7 , type = "both")

var_modell1 <- VAR(train_ts, p=1, type = "both")
summary(var_model)



?VAR
roots(var_model)
FCVAR_roots(roots(var_model))
var_model$varresult$income_ex$
v_forecast<- predict(var_model, n.ahead = test_size)
acf(var_model$varresult$income_ex$residuals)


accuracy(v_forecast$fcst$direct_costs[,1], test_data$direct_costs)

notting_forecast_2 <- apply_var_model(train_data, variables_to_include, 12, difference = FALSE)

notting_forecast_2

leeds_forecast<- apply_var_model(train_data, variables_to_include, 12, difference = TRUE)


accuracy(v_forecast$fcst$direct_costs[,1], test_ts[,2])

plot(v_forecast$fcst$overheads[,1])

s<- summary(var_model)

s$varresult$income_ex$fstatistic

var_model_log <- VAR(train, season = 12, lag.max = 10, type = "both")

n_forecast <-12


cost_centre <- 'PM_Sheffield'

subset_data <- subset(df_1, Cost_Centre == cost_centre)

# Split the data into training and test sets
train_data <- head(subset_data, -n_forecast)
test_data <- tail(subset_data, n_forecast)


train_data <- subset_data[, variables_to_include, drop = FALSE]
train_ts <- ts(train_data, start = c(year(min(subset_data$Date))
                                     , month(min(subset_data$Date))), frequency = 12)
##################################################################################


varinfo <-extract_var_model_info(var_models)
proc_model

arch.test(proc_model)

var_roots <- extract_var_roots_list(var_models)

var_arch_stability <- check_var_stability_and_arch(var_models)


var_info <- calculate_f_stat_p_values(varinfo)


var_tests<- add_serial_test_results(var_arch_stability, var_models)

file_name <- "var_arch_stability.csv"

# Save the dataframe to a CSV file
write.csv(var_arch_stability, file_name, row.names = FALSE)

# Print a message indicating the file was saved successfully
cat("Dataframe saved to", file_name, "successfully.\n")