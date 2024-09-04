library(tidyr)
library(dplyr)


df_long4 <- data_with_f4 %>%
  # Gather all actual values
  gather(key = "variable", value = "actual", income_ex, direct_costs, overheads, total_headcount) %>%
  
  # Gather all forecast values (for models and manager)
  gather(key = "forecast_type", value = "forecast", 
         income_ex_fcst12, direct_costs_fcst12, overheads_fcst12, total_headcount_fcst12,
         income_ex_baggedets_forecast, direct_costs_baggedets_forecast, overheads_baggedets_forecast, total_headcount_baggedets_forecast,
         income_ex_arima_forecast, direct_costs_arima_forecast, overheads_arima_forecast, total_headcount_arima_forecast,
         income_ex_ets_forecast, direct_costs_ets_forecast, overheads_ets_forecast, total_headcount_ets_forecast,
         income_ex_baggedarima_forecast, direct_costs_baggedarima_forecast, overheads_baggedarima_forecast, total_headcount_baggedarima_forecast,
         income_ex_nnetar_forecast, direct_costs_nnetar_forecast, overheads_nnetar_forecast, total_headcount_nnetar_forecast,
         income_ex_grnn_forecast, direct_costs_grnn_forecast, overheads_grnn_forecast, total_headcount_grnn_forecast,
         income_ex_var_forecast, direct_costs_var_forecast, overheads_var_forecast, total_headcount_var_forecast) %>%
  
  # Filter out mismatches between actual and forecast variables
  filter(gsub("_fcst12|_baggedets_forecast|_arima_forecast|_ets_forecast|_baggedarima_forecast|_nnetar_forecast|_grnn_forecast|_var_forecast", "", forecast_type) == variable) %>%
  
  # Create a new column to differentiate between manager and model forecasts
  mutate(model = case_when(
    grepl("_fcst12", forecast_type) ~ "Manager",
    grepl("_baggedets_forecast", forecast_type) ~ "BaggedETS",
    grepl("_arima_forecast", forecast_type) ~ "ARIMA",
    grepl("_baggedarima_forecast", forecast_type) ~ "BaggedARIMA",
    grepl("_ets_forecast", forecast_type) ~ "ETS",
    grepl("_nnetar_forecast", forecast_type) ~ "NNETAR",
    grepl("_grnn_forecast", forecast_type) ~ "GRNN",
    grepl("_var_forecast", forecast_type) ~ "VAR"
  )) %>%
  
  # Clean up the forecast_type column to just keep the variable name
  mutate(variable = gsub("_fcst12|_baggedets_forecast|_arima_forecast|_ets_forecast|_baggedarima_forecast|_nnetar_forecast|_grnn_forecast|_var_forecast", "", forecast_type)) %>%
  
  dplyr:: select(Date, Cost_Centre, Division, Region, variable, actual, model, forecast) %>%
  
  arrange(Cost_Centre, variable, model, Date)
