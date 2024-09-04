library(ggplot2)
library(vars)


filtered_df <- var_forecasts %>%
  filter(Cost_Centre == "...") %>%
  dplyr:: select(Date, 'direct_costs')


filtered_df$Date <- as.Date(filtered_df$Date, format="%Y-%m-%d")


ts_forecast <- ts(filtered_df[[2]], start = c(year(min(filtered_df$Date))
                                              , month(min(filtered_df$Date))), frequency = 12)


filtered_actual_df <- df_ok1 %>%
  filter(Cost_Centre == "...") %>%
  filter(Date >= as.Date("2020-05-01")) %>%
  dplyr:: select(Date, 'direct_costs')

# Step 2: Convert the Date column to Date format (if not already in Date format)
filtered_actual_df$Date <- as.Date(filtered_actual_df$Date, format="%Y-%m-%d")

# Step 3: Create a time series object from the filtered actual values
ts_actual <- ts(filtered_actual_df[[2]], start = c(year(min(filtered_actual_df$Date)),
                                                   month(min(filtered_actual_df$Date))), frequency = 12)



start_date <- as.Date("2020-05-01")

numeric_columns <- c("income_ex", "direct_costs", "overheads")

# Define custom names for the plots
custom_names <- c("Income", "Direct Costs", "Overheads")


create_ts_plot <- function(column_name, custom_name, forecasts, actuals) {
  # Filter forecast data
  forecast_filtered <- forecasts %>%
    filter(Cost_Centre == cost_centre) %>%
    dplyr:: select(Date, all_of(column_name))
  
  # Filter actual data
  actual_filtered <- actuals %>%
    filter(Cost_Centre == cost_centre) %>%
    filter(Date >= start_date) %>%
    dplyr:: select(Date, all_of(column_name))
  
  # Convert Date columns to Date format
  forecast_filtered$Date <- as.Date(forecast_filtered$Date, format="%Y-%m-%d")
  actual_filtered$Date <- as.Date(actual_filtered$Date, format="%Y-%m-%d")
  
  # Create ts objects
  ts_forecast <- ts(forecast_filtered[[2]], start = c(year(min(forecast_filtered$Date)), month(min(forecast_filtered$Date))), frequency = 12)
  ts_actual <- ts(actual_filtered[[2]], start = c(year(min(actual_filtered$Date)), month(min(actual_filtered$Date))), frequency = 12)
  
  # Create the plot using autoplot and autolayer
  p <- autoplot(ts_actual, series = NULL) +
    autolayer(ts_forecast, series = NULL, color = 'red') +
    labs(title = paste(custom_name)) +
    ylab('') +
    xlab('') +
    theme(legend.position = "none")
  
  return(p)
}

cost_centre <- "..."
# Generate plots for each numeric column with corresponding custom names
plots <- lapply(seq_along(numeric_columns), function(i) {
  create_ts_plot(numeric_columns[i], custom_names[i], var_forecasts, df_ok1)
})

# Arrange the plots side by side
grid.arrange(grobs = plots, ncol = 3, bottom = 'Monthly Timeline',
             left = 'Actual vs Forecast')



#roots# Plot ACFs for VAR model with lag order 1 (Top Row)
acf(var_modell1$varresult$income_ex$residuals, main = 'Income', ylab = 'Lag Order 1', xlab = '')
acf(var_modell1$varresult$direct_costs$residuals, main = 'Direct Costs', ylab = '', xlab = '')
acf(var_modell1$varresult$overheads$residuals, main = 'Overheads', ylab = '', xlab = '')

# Plot ACFs for VAR model with lag order 3 (Bottom Row)
acf(var_modell5$varresult$income_ex$residuals, main = '', ylab = 'Lag Order 5', xlab = 'Lag')
acf(var_modell5$varresult$direct_costs$residuals, main = '', ylab = '', xlab = 'Lag')
acf(var_modell5$varresult$overheads$residuals, main = '', ylab = '', xlab = 'Lag')




dev.off()

par(mfrow =c(1,1))

eigenvalues <- roots(var_model, modulus = FALSE)

# Create the plot
plot(Re(eigenvalues), Im(eigenvalues), xlab = "Real Part", ylab = "Imaginary Part", 
     main = "Eigenvalues of the companion matrix", xlim = c(-1, 1), ylim = c(-1, 1), 
     pch = 19, col = "blue", asp = 1)

# Draw the unit circle
symbols(0, 0, circles = 1, add = TRUE, inches = FALSE, fg = "red")

abline(h = 0, v = 0, lty = 2)




