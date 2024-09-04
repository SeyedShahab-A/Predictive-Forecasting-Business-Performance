library(panelvar)
library(dplyr)
library(readr)
library(zoo)
library(bvartools)
library(plm)
library(tseries)
library(forecast)
library(vars)

source('modules/impute_missing_data.R')
source('modules/forecast_pvar.R')
source('modules/calculate_mean_accuracy.R')


data <- read_csv("Combined files/combined_file_fcst2020added.csv")


data$Date <- as.Date(data$Date, format = "%Y-%m-%d")
data$Date <- as.factor(data$Date)
data$date_ID <- as.numeric(data$Date)


duplicates <- duplicated(df_ok)

# Display duplicate rows
duplicate_rows <- df_ok[duplicates, ]
print(duplicate_rows, n=35)

duplicate_rows$Date

df_ok <- df_ok[!duplicated(df_ok), ]

sum(duplicated(df_ok))

select_df <- df_ok


select_df$Cost_Centre <- as.factor(select_df$Cost_Centre)
select_df$Cost_Centre_ID <- as.numeric(select_df$Cost_Centre)

select_df$Date <- as.factor(select_df$Date)
select_df$date_ID <- as.numeric(select_df$Date)


data
data_model <- as.data.frame(select_df)


data_model$Date <- as.Date(data_model$Date)

last_date <- max(data_model$Date)


start_test_date <- as.Date(format(last_date, "%Y-%m-01")) - months(11)



# Split the data into training and test
train_data <- data_model %>%
  filter(Date < start_test_date)

test_data <- data_model %>%
  filter(Date >= start_test_date)

train_data

train <- train_data[,c("income_ex", "direct_costs", "overheads","Cost_Centre_ID","date_ID")]

Andrews_Lu_MMSC(pvar5_1)


################################################################################

model_list <- list()
for (i in 1:10){
  
  model_name <- paste0('pvar', i, '_1')
  model <- pvargmm(
    dependent_vars = c("income_ex", "direct_costs", 'overheads'),
    lags = i,
    data = train,
    transformation = 'fd',
    panel_identifier = c("Cost_Centre_ID", "date_ID"),
    system_instruments = TRUE,
    max_instr_dependent_vars = 60,
    steps = c("onestep"),
    collapse = TRUE
  )
  AIC <- Andrews_Lu_MMSC(model)
  assign(model_name, model)
  model_list[[model_name]] <- model
  model_list[[model_name]]['AIC'] <- AIC$MMSC_BIC
}


summary(pvar4_1)
stability(pvar4_1)

pvar9_fcst<- forecast_all_cost_centres(pvar9_1, train_data, test_data, 
                                       variables_to_include, back_transform = FALSE)
pvar9_acc<- pvar9_fcst$accuracy

calculate_mean_accuracy(pvar9_acc, 'direct_costs')

##################################################################################
#log transformation
log_train_data <- train_data %>%
  mutate(across(all_of(numeric_vars), log))

log_train <- log_train_data[,c("income_ex", "direct_costs", "overheads","Cost_Centre_ID","date_ID")]

sum(is.na(log_train$overheads))

log_train_data <- impute_missing_values(log_train_data, 'overheads')
nans_overhead_centres <- unique(log_train_data$Cost_Centre[is.na(log_train_data$overheads)])
nans_overhead_centres


?pvargmm

model_list <- list()
for (i in 1:10){
  
  model_name <- paste0('pvar', i, '_log')
  model <- pvargmm(
    dependent_vars = c("income_ex", "direct_costs", 'overheads'),
    lags = i,
    data = log_train,
    transformation = 'fd',
    panel_identifier = c("Cost_Centre_ID", "date_ID"),
    system_instruments = FALSE,
    system_constant = FALSE,
    max_instr_dependent_vars = 20,
    min_instr_dependent_vars = 2L,
    steps = c("onestep"),
    collapse = TRUE
  )
  AIC <- Andrews_Lu_MMSC(model)
  assign(model_name, model)
  model_list[[model_name]] <- model
  model_list[[model_name]]['AIC'] <- AIC$MMSC_BIC
}

model_list$pvar5_log$AIC

summary(pvar4_log)
stability(pvar4_1)

plot(pvar8_log)

pvar8_fcst<- forecast_all_cost_centres(pvar8_log, log_train_data, test_data, 
                                       variables_to_include, back_transform = TRUE)
pvar8_acc<- pvar8_fcst$accuracy

calculate_mean_accuracy(pvar8_acc, 'overheads')



##################################################################################

pdata_model <- pdata.frame(log_train, index = c("Cost_Centre_ID", 'date_ID'))

# LLC Test for income_ex
llc_income_ex <- purtest(pdata_model$income_ex, exo = "trend", pmax = 2, test = "ips")
summary(llc_income_ex)

# LLC Test for direct_costs
llc_direct_costs <- purtest(pdata_model$direct_costs, exo = "trend", pmax = 2, test = "levinlin")
summary(llc_direct_costs)

# LLC Test for overheads
llc_overheads <- purtest(pdata_model$overheads, exo = "trend", pmax = 2, test = "levinlin")
summary(llc_overheads)

###################################################################################

summary(varone)
coefs <- coef(pvar3_1)

###################################################################################
variables <- c("income_ex", "direct_costs", "overheads")
length(variables)

cost_centre <- '...'



subset_data <- subset(train_data, Cost_Centre == cost_centre)
test_subset_shef <- subset(test_data, Cost_Centre == cost_centre)


shef_forecasts <- forecast_pvar_fd(pvar4_1, subset_data, variables_to_include, 
                                     n_forecast = 12, back_transform = FALSE)



forecasts_df_shef <- as.data.frame(shef_forecasts)
forecasts_df_shef$Cost_Centre <- cost_centre
forecasts_df_shef$Date <- test_subset_shef$Date




n_forecast <- 12
for (var in variables_to_include) {
  accuracy_metrics <- accuracy(forecasts_df_shef[[var]], test_subset_shef[[var]])
  
  # Create a data frame for accuracy metrics
  accuracy_df_shef <- data.frame(
    Cost_Centre = cost_centre,
    #Division = unique(train_subset$Division),
    Variable = var,
    Start_Date = min(test_subset_shef$Date),
    Test_Size = n_forecast,
    Model = "PVAR",  
    ME = accuracy_metrics[1, "ME"],
    RMSE = accuracy_metrics[1, "RMSE"],
    MAE = accuracy_metrics[1, "MAE"],
    MPE = accuracy_metrics[1, "MPE"],
    MAPE = accuracy_metrics[1, "MAPE"],
    MASE = NA 
  )
  
  # Append to the accuracy list
  accuracy_list_shef[[length(accuracy_list_shef) + 1]] <- accuracy_df_shef
}

all_accuracy_df_shef <- do.call(rbind, accuracy_list_shef)


all_accuracy_df_shef
accuracy_list_shef
names(accuracy_metrics) <- variables

accuracy_metrics


coefs <- coef(pvar3_log)

summary(pvar3_log)

#####################################################################################[]
lag_vars <- grep("^fd_lag[0-9]+_", colnames(coefs), value = TRUE)
max_lag <- max(as.numeric(gsub("fd_lag([0-9]+)_.*", "\\1", lag_vars)))

lag_vars
max_lag

subset_data <- subset(log_train_data, Cost_Centre == '...')



forecast_pvar_fd(pvar3_log,subset_data, variables_to_include, back_transform = TRUE)
numeric_columns <- c("income_ex", "direct_costs", "overheads")  
data_for_forecasting <- subset_data[, numeric_columns, drop = FALSE]
n_forecast <- 12


variables <- variables_to_include
subset(test_data, Cost_Centre== '...')

diff_data <- lapply(data_for_forecasting, diff, 1)
diff_data <- as.data.frame(diff_data) 

diff_data

level_forecasts[1, 'income_ex']

fd_forecasts <- matrix(NA, nrow = n_forecast, ncol = length(numeric_columns))
colnames(fd_forecasts) <- numeric_columns

level_forecasts <- matrix(NA, nrow = 12, ncol = length(variables))
colnames(level_forecasts) <- variables


history
history <- tail(diff_data, max_lag)
last_known_levels <- tail(data_for_forecasting, 1)

last_known_levels

new_fd_forecast <- numeric(length(numeric_columns))
new_level_forecast <- numeric(length(numeric_columns))

as.numeric(gsub("fd_lag([0-9]+)_.*", "\\1", lag_vars[1]))
var <- 'overheads'
fd_forecast_value <- coefs[paste0("fd_", var), "const"]

fd_forecast_value



for (lag_var in lag_vars) {
  # Extract the lag number and the variable name
  lag_num <- as.numeric(gsub("fd_lag([0-9]+)_.*", "\\1", lag_var))
  orig_var_name <- gsub("fd_lag[0-9]+_", "", lag_var)
  
  # Get the corresponding lagged value from history (for first differences)
  lag_value <- history[nrow(history) - lag_num + 1, orig_var_name]
  
  # Add the contribution of this lagged variable to the forecast
  fd_forecast_value <- fd_forecast_value + coefs[paste0("fd_", var), lag_var] * lag_value
}

fd_forecast_value
new_fd_forecast[numeric_columns == var] <- fd_forecast_value

new_fd_forecast


new_level_forecast[numeric_columns == var] <- last_known_levels[var] + fd_forecast_value
new_level_forecast[numeric_columns == var] <- level_forecasts[3 - 1, var] + fd_forecast_value

new_level_forecast
fd_forecasts[3, ] <- new_fd_forecast
level_forecasts[3, ] <- new_level_forecast

history <- rbind(history[-1, , drop = FALSE], new_fd_forecast)

level_forecasts[1,] <- c(473.4602, 306.4512, 87.92758)
level_forecasts

level_forecasts <- matrix(NA, nrow = 12, ncol = length(variables))
colnames(level_forecasts) <- variables

for (var in variables){
  level_forecasts[1, var]<- 1
}
