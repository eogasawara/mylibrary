source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myTimeseries.R")

load_series <- function(name) {
  link <- url(sprintf("https://raw.githubusercontent.com/eogasawara/mylibrary/master/data/time-series/%s.RData", name))
  x <- get(load(link))
  return(x)  
}

train_test <- function(x, model, train, test, steps_ahead) {
  model <- ts_train(model, train)
  
  io <- ts_sw_project(test)
  
  prediction <- ts_predict(model, io$input)
  
  model <- ts_test(model, test, steps_ahead = steps_ahead)
  
  print(sprintf("%s %.2f%%", class(model)[1], 100*model$test_smape))
  
  return(model)
}

if (TRUE) {
  x <- load_series("sin")
  
  ttx <- ts_train_test(x, test_size=5, sw_size=0)
  train_test(x, model=ts_arima(), ttx$train, ttx$test, steps_ahead = 5)
  
  preprocess <- ts_gminmax()
  ttx <- ts_train_test(x, test_size=5, sw_size=10)
  train_test(x, model=ts_nnet(preprocess, input_size=4), ttx$train, ttx$test, steps_ahead = 5)
  train_test(x, model=ts_svm(preprocess, input_size=4), ttx$train, ttx$test, steps_ahead = 5)
  train_test(x, model=ts_rf(preprocess, input_size=4), ttx$train, ttx$test, steps_ahead = 5)
  train_test(x, model=ts_elm(preprocess, input_size=4), ttx$train, ttx$test, steps_ahead = 5)
  train_test(x, model=ts_mlp(preprocess, input_size=4), ttx$train, ttx$test, steps_ahead = 5)
  train_test(x, model=ts_tensor_cnn(preprocess, input_size=4), ttx$train, ttx$test, steps_ahead = 5)
  train_test(x, model=ts_tensor_lstm(preprocess, input_size=4), ttx$train, ttx$test, steps_ahead = 5)
}

