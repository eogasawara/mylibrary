#source("myTSRegression.R")

load_series <- function(name) {
  #link <- url(sprintf("https://raw.githubusercontent.com/eogasawara/mylibrary/master/data/time-series/%s.RData", name))
  link <- sprintf("./data/time-series/%s.RData", name)
  x <- get(load(link))
  return(x)  
}

train_test <- function(x, model, sw, test_size, steps_ahead) {
  ts <- ts_data(x, sw)
  
  samp <- ts_sample(ts, test_size)

  io_train <- ts_projection(samp$train)

  model <- prepare(model, x=io_train$input, y=io_train$output)
  
  adjust <- action(model, io_train$input)
  ev_adjust <- tsregression_evaluation(io_train$output, adjust)
  print(head(ev_adjust$metrics))

  io_test <- ts_projection(samp$test)
  
  prediction <- action(model, io_test$input)
  ev_prediction <- tsregression_evaluation(io_test$output, prediction)
  print(head(ev_prediction$metrics))
  
  print(sprintf("%s %.2f", class(model)[1], 100*model$test_smape))
  
  plot(model, y=c(io_train$output, io_test$output), yadj=adjust, ypre=prediction)
  
  return(model)
}

if (TRUE) {
  x <- load_series("sin")
  sahead <- 1
  tsize <- 1
  swsize <- 10
  preproc <- ts_gminmax()
  #train_test(x, model=ts_arima(), 0, test_size = tsize, steps_ahead = sahead)
  #train_test(x, model=tsreg_emlp_dir(4), 0, test_size = tsize, steps_ahead = sahead)
  #train_test(x, model=tsreg_eelm_dir(4), 0, test_size = tsize, steps_ahead = sahead)
  
  #train_test(x, model=ts_nnet(preproc, input_size=4), sw = swsize, test_size = tsize, steps_ahead = sahead)
  #train_test(x, model=ts_svm(preproc, input_size=4), sw = swsize, test_size = tsize, steps_ahead = sahead)
  #train_test(x, model=ts_rf(preproc, input_size=4), sw = swsize, test_size = tsize, steps_ahead = sahead)
  #train_test(x, model=ts_elm(preproc, input_size=4), sw = swsize, test_size = tsize, steps_ahead = sahead)
  #train_test(x, model=ts_tensor_cnn(preproc, input_size=4), sw = swsize, test_size = tsize, steps_ahead = sahead)
  train_test(x, model=ts_tensor_lstm(preproc, input_size=4), sw = swsize, test_size = tsize, steps_ahead = sahead)
}

