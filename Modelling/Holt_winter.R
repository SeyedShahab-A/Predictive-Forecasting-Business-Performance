# Holt_winter.R


library(readr)
library(dplyr)
library(zoo)
library(lubridate)
library(forecast)
library(ggplot2)

# Source the external scripts
source('modules/data_splitting.R')

unique(data1[data1$Division == 'Project management',]$Cost_Centre)
data <- read_csv("Combined files/combined_file_fcst12.csv")

data$Date <- as.Date(data$Date)




select_df <- data1 %>%
  filter(Division == 'Project management', Cost_Centre == '...')


date_column <- 'Date'

start_year <- year(min(select_df[[date_column]]))
start_month <- month(min(select_df[[date_column]]))
end_year <- year(max(select_df[[date_column]]))
end_month <- month(max(select_df[[date_column]]))

ts_dc <- ts(select_df[['direct_costs']], frequency = 12,
                start = c(start_year, start_month),
                end = c(end_year, end_month))



split <- split_data(ts_dc, test_size = 12)

train <- split$train
test <- split$test
length(test)


ts_dc_fcst <- ts(select_df[['direct_costs_fcst12']], frequency = 12,
              start = c(2023, 5),
            end = c(2024, 4))




?HoltWinters

fcnaive <- naive(train, h = 12)

holt_lin <- holt(train, h=12)

hw_ad<- hw(train, h=12)

hw_mul<- hw(train, h=12, seasonal='multiplicative')




time_point <- max(time(train))
 
autoplot(ts_dc) +
  autolayer(holt_lin$mean, series="Holt Linear", PI=FALSE) +
  autolayer(hw_ad$mean, series="Holt Winter Additive", PI=FALSE) +
  #autolayer(ts_dc_fcst, series="Managers", PI=FALSE) +
  autolayer(hw_mul$mean, series="Holt-Winters Multiplicative", PI=FALSE) +
  ggtitle("Overlayed Forecasts on the Last 12 month of Data") +
  xlab("Time") +
  ylab("Direct Costs") +
  guides(colour=guide_legend(title="Forecast Model")) +
  theme(legend.position="bottom") +
  geom_vline(xintercept=as.numeric(time_point)+0.1, linetype="dashed", color="grey", linewidth=1)



par(mfrow = c(1, 3))

# Plot the ACF plots
acf(holt_lin$residuals, main = "Holt Linear")
acf(hw_ad$residuals, main = "Holt Winter Additive")
acf(hw_mul$residuals, main = "Holt Winter Multiplicitive")
