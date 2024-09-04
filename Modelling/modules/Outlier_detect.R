detect_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  
  lower_bound <- Q1 - 3 * IQR
  upper_bound <- Q3 + 3 * IQR
  
  return(x < lower_bound | x > upper_bound)
}



outliers_df <- df_lok1 %>%
  group_by(Cost_Centre) %>%
  filter(if_any(where(is.numeric), detect_outliers))

print(outliers_df)