source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myTimeseries.R")

load_series <- function(name) {
  link <- url(sprintf("https://raw.githubusercontent.com/eogasawara/mylibrary/master/data/time-series/%s.RData", name))
  x <- get(load(link))
  return(x)  
}

#==================== No decomposition ======================
train_test <- function(x, model, test_size=5, steps_ahead, sw_size=0) {
  
  ttx <- ts_train_test(x, test_size, sw_size)
  
  train <- ttx$train
  test <- ttx$test
  
  model <- ts_train(model, train)
  
  model <- ts_test(model, test, steps_ahead = steps_ahead)
  
  
  smape_test <- 100*model$test_smape
  smape_train <- 100*model$train_smape
  print(sprintf("%s %.2f%%", class(model)[1], 100*model$test_smape))
  
  ts_plot_series(c(model$train_value, model$test_value), model$train_pred, model$test_pred, class(model)[1])
  
  return(cbind(smape_train=smape_train,smape_test=smape_test))
}

#==================== EMD decomposition ======================
train_test_emd <- function(x, model, test_size=5, steps_ahead, sw_size=0) {
  #browser()
  
  #preprocess with EMD
  x_decomp <- emd(x, num_imfs=0, S_number=4L, num_siftings=50L, meaningfulImfs=NULL, h=1)
  
  pred <- list()
  
  for(imf in 1:length(x_decomp)){
    
    x_imf <- x_decomp[[imf]]
    
    ttx <- ts_train_test(x_imf, test_size, sw_size)
    
    x_train <- ttx$train
    x_test <- ttx$test
    
    model <- ts_train(model, x_train)
    model <- ts_test(model, x_test, steps_ahead = steps_ahead)
    
    pred[[imf]] <- c(model$train_pred,model$test_pred)
  }
  
  #postprocess the predictions
  pred <- emd.rev(pred)
  train_pred <- pred[1:(length(pred)-test_size)]
  test_pred <- tail(pred,test_size)

  train <- head(tail(x,length(pred)),length(pred)-test_size)
  test <- tail(x,test_size)
  
  smape_test <- TSPred::sMAPE(test, test_pred)*100
  smape_train <- TSPred::sMAPE(train, train_pred)*100
  print(sprintf("%s %.2f%%", class(model)[1], smape_test))
  
  ts_plot_series(c(train,test), train_pred, test_pred, class(model)[1])
  
  return(cbind(smape_train=smape_train,smape_test=smape_test))
}

#================= Wavelet transform decomposition ===================
train_test_wt <- function(x, model, test_size=5, steps_ahead, sw_size=0) {
  #browser()
  ttx <- ts_train_test(x, test_size)
  
  train <- ttx$train
  test <- ttx$test
  
  #preprocess with Wavelet transform
  x_decomp <- WaveletT(x,level=NULL,filter=c("haar", "d4", "la8", "bl14", "c6"),boundary="periodic",h=1)
  
  pred <- list()
  
  for(imf in 1:length(x_decomp)){
    x_imf <- x_decomp[[imf]]
    
    ttx <- ts_train_test(x_imf, test_size, sw_size)
    
    x_train <- ttx$train
    x_test <- ttx$test
    
    model <- ts_train(model, x_train)
    model <- ts_test(model, x_test, steps_ahead = steps_ahead)
    
    pred[[imf]] <- c(model$train_pred,model$test_pred)
  }
  
  #postprocess the predictions
  pred <- WaveletT.rev(pred=pred,attr(x_decomp,"wt_obj"))
  
  train_pred <- pred[1:(length(pred)-test_size)]
  test_pred <- tail(pred,test_size)
  
  train <- head(tail(x,length(pred)),length(pred)-test_size)
  test <- tail(x,test_size)
  
  smape_test <- TSPred::sMAPE(test, test_pred)*100
  smape_train <- TSPred::sMAPE(train, train_pred)*100
  print(sprintf("%s %.2f%%", class(model)[1], smape_test))
  
  ts_plot_series(c(train,test), train_pred, test_pred, class(model)[1])
  
  return(cbind(smape_train=smape_train,smape_test=smape_test))
}

x <- load_series("sin")

library(TSPred)
data("CATS")
x <- CATS[,1]


mdls <- list("ts_arima()")
funcs <- list("train_test","train_test_emd","train_test_wt")

benchmark_exp <- function(x,funcs,mdls,par){
  #browser()
  benchmark <- data.frame()
  for(f in funcs){
    for(mdl in mdls){
      res <- do.call(f,c(list(x),list(model=eval(parse(text=mdl))), c(par)))
      benchmark <- rbind(benchmark,cbind(func=f,model=mdl,res))
    }
  }
  return(benchmark)
}

benchmark_exp(x,funcs,mdls,list(test_size=5, steps_ahead = 5))

if (TRUE) {
  x <- load_series("sin")
  
  train_test(x, model=ts_arima(), test_size=5, steps_ahead = 5)
  train_test(x, model=ts_eelm_dir(4), test_size=5, steps_ahead = 5)
  train_test(x, model=ts_emlp_dir(4), test_size=5, steps_ahead = 5)
  
  preprocess <- ts_gminmax()
  require("e1071")
  train_test_wt(x, model=ts_nnet(preprocess, input_size=4), test_size=5, steps_ahead = 5, sw_size=10)
  train_test(x, model=ts_svm(preprocess, input_size=4), test_size=5, steps_ahead = 5, sw_size=10)
  train_test(x, model=ts_rf(preprocess, input_size=4), test_size=5, steps_ahead = 5, sw_size=10)
  train_test(x, model=ts_elm(preprocess, input_size=4), test_size=5, steps_ahead = 5, sw_size=10)
  train_test(x, model=ts_mlp(preprocess, input_size=4), test_size=5, steps_ahead = 5, sw_size=10)
  train_test(x, model=ts_tensor_cnn(preprocess, input_size=4), test_size=5, steps_ahead = 5, sw_size=10)
  train_test(x, model=ts_tensor_lstm(preprocess, input_size=4), test_size=5, steps_ahead = 5, sw_size=10)
}