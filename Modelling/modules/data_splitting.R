# data_splitting.R

split_data <- function(ts_data, train_ratio = 0.8, test_size = NULL) {
  n <- length(ts_data)
  
  if (!is.null(test_size)) {
    train_size <- n - test_size
  } else {
    train_size <- floor(train_ratio * n)
  }
  
  train_data <- ts(ts_data[1:train_size], frequency = frequency(ts_data), 
                   start = start(ts_data))
  test_data <- ts(ts_data[(train_size + 1):n], frequency = frequency(ts_data),
                  start = c(tsp(ts_data)[1] + train_size / frequency(ts_data)))
  
  return(list(train = train_data, test = test_data))
}
