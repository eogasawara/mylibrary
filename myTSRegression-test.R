#source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myTSRegression.R")

load_series <- function(name) {
  link <- url(sprintf("https://raw.githubusercontent.com/eogasawara/mylibrary/master/data/time-series/%s.RData", name))
  x <- get(load(link))
  return(x)  
}

train_test <- function(x, model, sw, test_size, steps_ahead) {
  ts <- ts_data(x, sw)
  
  samp <- ts_sample(ts, test_size)

  io_train <- ts_projection(samp$train)

  model <- prepare(model, x=io_train$input, y=io_train$output)
  
  adjust <- action(model, io_train$input)
  ev_adjust <- ts_regression_evaluation(io_train$output, adjust)
  print(head(ev_adjust$metrics))

  io_test <- ts_projection(samp$test)
  
  prediction <- action(model, io_test$input)
  ev_prediction <- ts_regression_evaluation(io_test$output, prediction)
  print(head(ev_prediction$metrics))
  
  #model <- ts_test(model, samp$test, steps_ahead = steps_ahead)
  
  print(sprintf("%s %.2f", class(model)[1], 100*model$test_smape))
  
  plot(model, y=c(io_train$output, io_test$output), yadj=adjust, ypre=prediction)
  
  return(model)
}

if (TRUE) {
  x <- load_series("sin")
  
  #train_test(x, model=ts_arima(), 0, 5, steps_ahead = 5)
  #train_test(x, model=tsreg_emlp_dir(4), 0, 5, steps_ahead = 5)
  #train_test(x, model=tsreg_eelm_dir(4), 0, 5, steps_ahead = 5)
  
  train_test(x, model=ts_nnet(ts_gminmax(), input_size=4), 10, 5, steps_ahead = 5)
  #train_test(x, model=ts_svm(ts_gminmax(), input_size=4), 10, 5, steps_ahead = 5)
  #train_test(x, model=ts_rf(ts_gminmax(), input_size=4), 10, 5, steps_ahead = 5)
  #train_test(x, model=ts_elm(ts_gminmax(), input_size=4), 10, 5, steps_ahead = 5)
  #train_test(x, model=ts_mlp(ts_gminmax(), input_size=4), 10, 5, steps_ahead = 5)
  #train_test(x, model=ts_tensor_cnn(ts_gminmax(), input_size=4), 10, 5, steps_ahead = 5)
  #train_test(x, model=ts_tensor_lstm(ts_gminmax(), input_size=4), 10, 5, steps_ahead = 5)
}

