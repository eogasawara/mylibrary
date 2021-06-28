# version 1.1
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myTSRegression.R")

load_series <- function(name) {
  link <- url(sprintf("https://raw.githubusercontent.com/eogasawara/mylibrary/master/data/time-series/%s.RData", name))
  x <- get(load(link))
  return(x)  
}
x <- load_series("sin")
tsize <- 4
swsize <- 10

train_test <- function(x, model, sw, test_size, steps_ahead) {
  ts <- ts_data(x, sw)
  
  samp <- ts_sample(ts, test_size)
  
  io_train <- ts_projection(samp$train)
  
  model <- prepare(model, x=io_train$input, y=io_train$output)
  
  adjust <- action(model, io_train$input)
  ev_adjust <- evaluation.tsreg(io_train$output, adjust)
  print(head(ev_adjust$metrics))
  
  io_test <- ts_projection(samp$test)
  
  prediction <- action(model, x=io_test$input, steps_ahead=steps_ahead)
  output <- as.vector(io_test$output)
  if (steps_ahead > 1)
    output <- output[1:steps_ahead]
  ev_prediction <- evaluation.tsreg(output, prediction)
  print(head(ev_prediction$metrics))
  
  prep <- ""
  if (!is.null(model$preprocess))
    prep <- sprintf("-%s", class(model$preprocess)[1])    
  
  print(sprintf("%s%s %.2f", class(model)[1], prep, 100*ev_prediction$metrics$smape))
  
  yvalues <- c(io_train$output, io_test$output)
  plot(model, y=yvalues, yadj=adjust, ypre=prediction)
  return(model)
}

if (TRUE) {
  #1:tsize
  for (sahead in 1:tsize) {
    train_test(x, model=tsreg_arima(), 0, test_size = tsize, steps_ahead = sahead)
    train_test(x, model=tsreg_rf(ts_gminmax(), input_size=5, mtry=3, ntree=50), sw = swsize, test_size = tsize, steps_ahead = sahead)
    train_test(x, model=tsreg_mlp(ts_gminmax(), input_size=5, size=2,decay=0.00), sw = swsize, test_size = tsize, steps_ahead = sahead)
    train_test(x, model=tsreg_mlp(ts_gminmax_diff(), input_size=5, size=2,decay=0.00), sw = swsize, test_size = tsize, steps_ahead = sahead)
    train_test(x, model=tsreg_mlp(ts_swminmax(), input_size=5, size=2,decay=0.00), sw = swsize, test_size = tsize, steps_ahead = sahead)
    train_test(x, model=tsreg_mlp(ts_an(), input_size=5, size=2,decay=0.00), sw = swsize, test_size = tsize, steps_ahead = sahead)
    train_test(x, model=tsreg_svm(ts_gminmax(), input_size=5, epsilon=0.0, cost=80.00), sw = swsize, test_size = tsize, steps_ahead = sahead)
    train_test(x, model=tsreg_elm(ts_gminmax(), input_size=5, nhid=3,actfun="purelin"), sw = swsize, test_size = tsize, steps_ahead = sahead)
    train_test(x, model=tsreg_cnn(ts_gminmax(), input_size=5, neurons=16,epochs=200), sw = swsize, test_size = tsize, steps_ahead = sahead)
    train_test(x, model=tsreg_lstm(ts_gminmax(), input_size=5, neurons=32, epochs=200), sw = swsize, test_size = tsize, steps_ahead = sahead)
  }
}
