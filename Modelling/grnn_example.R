

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
library(tsfgrnn)

# Source the external scripts
source('modules/data_splitting.R')
source('modules/impute_missing_data.R')



sample_cost_centre <- "...."
sample_data <- data_h %>% filter(Cost_Centre == sample_cost_centre)


dts<- ts(sample_data$income_ex, start = c(year(min(sample_data$Date))
                                                 , month(min(sample_data$Date))), frequency = 12)

unique(data_h[data_h$Division == 'Project management',]$Cost_Centre)

variables_to_include <- c("income_ex", "direct_costs")

test_size <- 12

train_data <- sample_data[1:(nrow(sample_data) - test_size), ]
test_data <- sample_data[(nrow(sample_data) - test_size + 1):nrow(sample_data), ]

train_ts <- ts(train_data[, variables_to_include], start = c(year(min(train_data$Date))
                                                             , month(min(train_data$Date))), frequency = 12)
test_ts <- ts(test_data[, variables_to_include], start = c(year(min(test_data$Date))
                                                           , month(min(test_data$Date))), frequency = 12)
plot(train_ts)
pred <- grnn_forecasting(data ,lags= 'BE',  h = 12, transform = 'additive')

pred$prediction

accuracy(pred$prediction, test_ts[,1])
