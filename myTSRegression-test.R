#source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myTSRegression.R")

load_series <- function(name) {
  link <- url(sprintf("https://raw.githubusercontent.com/eogasawara/mylibrary/master/data/time-series/%s.RData", name))
  x <- get(load(link))
  return(x)  
}

train_test <- function(x, model, sw, test_size, steps_ahead) {
  ts <- ts_data(x, sw)
  
  samp <- ts_sample(ts, test_size)

  model <- ts_train(model, samp$train)
  
  io <- ts_projection(samp$test)
  
  prediction <- ts_predict(model, io$input)
  
  model <- ts_test(model, samp$test, steps_ahead = steps_ahead)
  
  print(sprintf("%s %.2f%%", class(model)[1], 100*model$test_smape))
  
  ts_plot_series(c(model$train_value, model$test_value), model$train_pred, model$test_pred, class(model)[1])
  
  return(model)
}

if (TRUE) {
  x <- load_series("sin")
  
  #train_test(x, model=ts_arima(), 0, 5, steps_ahead = 5)
  #train_test(x, model=ts_eelm_dir(4), 0, 5, steps_ahead = 5)
  #train_test(x, model=ts_emlp_dir(4), 0, 5, steps_ahead = 5)

  #train_test(x, model=ts_nnet(ts_gminmax(), input_size=4), 10, 5, steps_ahead = 5)
  #train_test(x, model=ts_svm(ts_gminmax(), input_size=4), 10, 5, steps_ahead = 5)
  #train_test(x, model=ts_rf(ts_gminmax(), input_size=4), 10, 5, steps_ahead = 5)
  #train_test(x, model=ts_elm(ts_gminmax(), input_size=4), 10, 5, steps_ahead = 5)
  #train_test(x, model=ts_mlp(ts_gminmax(), input_size=4), 10, 5, steps_ahead = 5)
  #train_test(x, model=ts_tensor_cnn(ts_gminmax(), input_size=4), 10, 5, steps_ahead = 5)
  #train_test(x, model=ts_tensor_lstm(ts_gminmax(), input_size=4), 10, 5, steps_ahead = 5)
}

