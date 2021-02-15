source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myNormalization.R")

# regression

tsregression <- function() {
  obj <- dal_transform()
  
  class(obj) <- append("tsregression", class(obj))    
  return(obj)
}

prepare.tsregression <- function(obj, x, y) {
  set.seed(1)
  obj <- start_log(obj)   
  return(obj)
}

action.tsregression <- function(obj, x) {
  return(x[,ncol(x)])
}

#class tsreg_arima

tsreg_arima <- function() {
  obj <- tsregression()
  
  class(obj) <- append("tsreg_arima", class(obj))  
  return(obj)
}

prepare.tsreg_arima <- function(obj, x, y = NULL) {
  obj <- prepare.tsregression(obj, x, y)
  
  loadlibrary("forecast")  
  obj$mdl <- auto.arima(x, allowdrift = TRUE, allowmean = TRUE) 
  
  obj <- register_log(obj)    
  return(obj)
}

action.tsreg_arima <- function(obj, x, y = NULL, steps_ahead=NULL) {
  loadlibrary("forecast")  
  if (!is.null(x) && (length(obj$mdl$x) == length(x)) && (sum(obj$mdl$x-x) == 0)){
    #get adjusted data
    pred <- obj$mdl$x - obj$mdl$residuals    
  }
  else {
    if (is.null(steps_ahead))
      steps_ahead <- length(x)
    pred <- forecast(obj$mdl, h = steps_ahead)
    pred <- pred$mean
  }
  return(pred)
}

# setup for sliding window

ts_invoke_prepare <- function(obj, x, y = NULL) {
  UseMethod("ts_invoke_prepare")
}

ts_invoke_action <- function(obj, x) {
  UseMethod("ts_invoke_action")
}

#class tsreg_sw

tsreg_sw <- function(preprocess, input_size) {
  obj <- tsregression()

  obj$preprocess <- preprocess
  obj$input_size <- input_size
  
  class(obj) <- append("tsreg_sw", class(obj))  
  return(obj)
}

ts_as_matrix <- function(data, input_size) {
  data <- data[,(ncol(data)-input_size+1):ncol(data)]
  return(data)
}

prepare.tsreg_sw <- function(obj, x, y) {
  obj <- prepare.tsregression(obj, x, y)
  
  set.seed(1)
  
  obj$preprocess <- prepare(obj$preprocess, x)
  
  x <- action(obj$preprocess, x)
  
  y <- action(obj$preprocess, x, y)
  
  obj <- ts_invoke_prepare(obj, ts_as_matrix(x, obj$input_size), y)
  
  obj <- register_log(obj)    
  return(obj)
}

action.tsreg_sw <- function(obj, x, steps_ahead=1) {
  if (steps_ahead == 1) {
    x <- action(obj$preprocess, x)
    y <- ts_invoke_action(obj, ts_as_matrix(x, obj$input_size))
    y <- deaction(obj$preprocess, x, y)
    return(y)
  }
  else {
    prediction <- NULL
    x <- x[1,]
    for (i in 1:steps_ahead) {
      x <- action(obj$preprocess, x)
      y <- ts_invoke_action(obj, ts_as_matrix(x, obj$input_size))
      x <- deaction(obj$preprocess, x)
      y <- deaction(obj$preprocess, x, y)
      x <- cbind(x[,2:ncol(X)], y)
      prediction <- c(prediction, y)
    }
    return(prediction)
  }
  return(prediction)
}

ts_invoke_action.tsreg_sw <- function(obj, x) {
  prediction <- predict(obj$mdl, x)  
  return(prediction)
}

#class tsreg_nnet

tsreg_nnet <- function(preprocess, input_size, neurons=NULL, decay=seq(0, 1, 0.02), maxit=1000) {
  obj <- tsreg_sw(preprocess, input_size)
  
  if (is.null(neurons))
    neurons <- unique(1:ceiling(input_size/3))
  obj$neurons <- neurons
  obj$decay <- decay
  obj$maxit <- maxit
  
  class(obj) <- append("tsreg_nnet", class(obj))  
  return(obj)
}

ts_invoke_prepare.tsreg_nnet <- function(obj, x, y) {
  loadlibrary("e1071")
  loadlibrary("nnet")  
  tuned <- tune(nnet, x, y, maxit=obj$maxit, trace=FALSE, ranges=list(decay=obj$decay, size=obj$neurons, linout=TRUE))
  obj$mdl <- tuned$best.model
  return(obj)
}


#class tsreg_svm

tsreg_svm <- function(preprocess, input_size, epsilon=seq(0,1,0.1), cost=seq(5,100,5), kernel="radial") {
  obj <- tsreg_sw(preprocess, input_size)

  obj$kernel <- kernel
  obj$epsilon <- epsilon
  obj$cost <- cost
  
  class(obj) <- append("tsreg_svm", class(obj))  
  return(obj)
}

ts_invoke_prepare.tsreg_svm <- function(obj, x, y) {
  loadlibrary("e1071")
  
  tuned <- tune(svm, x, y, ranges=list(epsilon=seq(0,1,0.1), cost=1:100))
  obj$mdl <- tuned$best.model
  return(obj)
}

#class tsreg_rf

tsreg_rf <- function(preprocess, input_size, mtry = NULL, ntree = seq(50, 500, 50)) {
  obj <- tsreg_sw(preprocess, input_size)
  
  if (is.null(mtry))
    mtry <- unique(1:ceiling(input_size/3))
  obj$mtry <- mtry
  obj$ntree <- ntree
  
  class(obj) <- append("tsreg_rf", class(obj))    
  return(obj)
}

ts_invoke_prepare.tsreg_rf <- function(obj, x, y) {
  loadlibrary("e1071")
  loadlibrary("randomForest")
  
  tuned <- tune(randomForest, x, y, ranges=list(mtry=obj$mtry, ntree=obj$ntree))
  obj$mdl <- tuned$best.model 

  return(obj)
}


#class tsreg_elm

tsreg_elm <- function(preprocess, input_size, nhid=NULL) {
  obj <- tsreg_sw(preprocess, input_size)
  
  if (is.null(nhid))
    nhid <- unique(1:ceiling(input_size/3))
  obj$nhid <- nhid

  class(obj) <- append("tsreg_elm", class(obj))    
  return(obj)
}

ts_invoke_prepare.tsreg_elm <- function(obj, x, y) {
  loadlibrary("e1071")
  loadlibrary("elmNNRcpp")
  
  obj$mdl <- elm_train(x, y, nhid = max(obj$nhid), actfun = 'purelin', init_weights = "uniform_positive", bias = FALSE, verbose = FALSE)
  
  return(obj)
}

ts_invoke_action.tsreg_elm <- function(obj, x) {
  prediction <- elm_predict(obj$mdl, x)
  return(prediction)
}


#class tsreg_cnn

tsreg_cnn <- function(preprocess, input_size, neurons=64, epochs = 1000) {
  obj <- tsreg_sw(preprocess, input_size)
  obj$neurons <- neurons
  obj$epochs <- epochs

  class(obj) <- append("tsreg_cnn", class(obj))    
  return(obj)
}

ts_invoke_prepare.tsreg_cnn <- function(obj, x, y) {
  print_dot_callback <- callback_lambda(
    on_epoch_end = function(epoch, logs) {
      if (epoch %% 800 == 0) cat("\n")
      if (epoch %% 10 == 0) cat(".")
    }
  )    
  
  loadlibrary("dplyr")
  loadlibrary("tfdatasets")
  loadlibrary("tensorflow")
  loadlibrary("keras")  
  
  xy <- data.frame(x)
  xy$t0 <- y
  
  set.seed(1)
  
  spec <- feature_spec(xy, t0 ~ . ) %>% 
    step_numeric_column(all_numeric(), normalizer_fn = scaler_standard()) %>% 
    fit()
  
  input <- layer_input_from_dataset(xy %>% select(-t0))
  
  output <- input %>% 
    layer_dense_features(dense_features(spec)) %>% 
    layer_dense(units = obj$neurons, activation = "relu") %>% 
    layer_dense(units = obj$neurons, activation = "relu") %>%
    layer_dense(units = 1) 
  
  model <- keras_model(input, output)
  
  model %>% 
    compile(loss = "mse", optimizer = optimizer_rmsprop(), 
            metrics = list("mean_absolute_error"))

  history <- model %>% fit(
    x = xy %>% select(-t0),
    y = xy$t0,
    epochs = obj$epochs,
    validation_split = 0.2,
    verbose = 0,
    callbacks = list(print_dot_callback)
  )  
  cat("\n")
  
  obj$mdl <- model
  
  return(obj)
}

ts_invoke_action.tsreg_cnn <- function(obj, x) {
  x <- data.frame(x)
  prediction <- (obj$mdl %>% predict(x))  
  return(prediction)
}

#class tsreg_lstm

tsreg_lstm <- function(preprocess, input_size, epochs = 1000) {
  obj <- tsreg_sw(preprocess, input_size)
  
  obj$epochs <- epochs
  
  class(obj) <- append("tsreg_lstm", class(obj))    
  return(obj)
}

ts_invoke_prepare.tsreg_lstm <- function(obj, x, y) {
  print_dot_callback <- callback_lambda(
    on_epoch_end = function(epoch, logs) {
      if (epoch %% 800 == 0) cat("\n")
      if (epoch %% 10 == 0) cat(".")
    }
  )    
  
  set.seed(1)
  batch.size <- 1
  size <- ncol(x)
  
  x <- array(as.vector(x), dim=(c(dim(x),1)))
  
  model <- keras_model_sequential()
  model %>%
    layer_lstm(units = 100,
               input_shape = c(size, 1),
               batch_size = batch.size,
               return_sequences = TRUE,
               stateful = TRUE) %>%
    layer_dropout(rate = 0.5) %>%
    layer_lstm(units = 50,
               return_sequences = FALSE,
               stateful = TRUE) %>%
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 1)
  
  model %>%
    compile(loss = 'mae', optimizer = 'adam')

  model %>% fit(x = x,
                y = y,
                batch_size = batch.size,
                epochs = obj$epochs,
                verbose = 0,
                shuffle = FALSE,
                callbacks = list(print_dot_callback))
  
  model %>% reset_states()
  cat("\n")
  
  obj$mdl <- model
  
  return(obj)
}

ts_invoke_action.tsreg_lstm <- function(obj, x) {
  x <- array(as.vector(x), dim=(c(dim(x),1)))
  batch.size <- 1
  prediction <- obj$mdl %>% predict(x, batch_size = batch.size) %>% .[,1]
  return(prediction)
}

# utility functions

# regression_evaluation
tsregression_evaluation <- function(values, prediction) {
  obj <- list(values=values, prediction=prediction)
  
  loadlibrary("TSPred")  
  
  obj$smape <- TSPred::sMAPE(values, prediction)  
  obj$mse <- TSPred::MSE(values, prediction)  
  
  obj$metrics <- data.frame(mse=obj$mse, smape=obj$smape)
  
  attr(obj, "class") <- "tsregression_evaluation"  
  return(obj)
}

plot.tsregression <- function(obj, y, yadj, ypre) {
  loadlibrary("TSPred")
  modelname <- class(obj)[1]
  ntrain <- length(yadj)
  smape_train <- TSPred::sMAPE(y[1:ntrain], yadj)*100
  smape_test <- TSPred::sMAPE(y[(ntrain+1):(ntrain+length(ypre))], ypre)*100
  par(xpd=TRUE)      
  plot(1:length(y), y, main = modelname, xlab = sprintf("time [smape train=%.2f%%], [smape test=%.2f%%]", smape_train, smape_test), ylab="value")
  lines(1:ntrain, yadj, col="blue")  
  lines((ntrain+1):(ntrain+length(ypre)), ypre, col="green")  
  par(xpd=FALSE)      
}