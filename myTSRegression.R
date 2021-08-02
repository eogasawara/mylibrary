# version 1.2
# depends myBasic.R
# depends myPreprocessing.R

### time series regression 

tsreg <- function() {
  obj <- list()
  attr(obj, "class") <- "tsreg"  
  return(obj)
}

train.tsreg <- function(obj, x, y) {
  obj <- start_log(obj)   
  return(obj)
}

#predict

predict.tsreg <- function(obj, x) {
  return(x[,ncol(x)])
}

#class tsreg_arima

tsreg_arima <- function() {
  obj <- tsreg()
  
  class(obj) <- append("tsreg_arima", class(obj))  
  return(obj)
}

train.tsreg_arima <- function(obj, x, y = NULL) {
  obj <- train.tsreg(obj, x, y)
  
  loadlibrary("forecast")  
  obj$model <- auto.arima(x, allowdrift = TRUE, allowmean = TRUE) 
  
  obj <- register_log(obj)    
  return(obj)
}

predict.tsreg_arima <- function(obj, x, y = NULL, steps_ahead=NULL) {
  loadlibrary("forecast")  
  if (!is.null(x) && (length(obj$model$x) == length(x)) && (sum(obj$model$x-x) == 0)){
    #get adjusted data
    pred <- obj$model$x - obj$model$residuals    
  }
  else {
    if (is.null(steps_ahead))
      steps_ahead <- length(x)
    if ((steps_ahead == 1) && (length(x) != 1)) {
      pred <- NULL
      model <- obj$model
      i <- 1
      while (i <= length(x)) {
        pred <- c(pred, forecast(model, h = 1)$mean)
        model <- auto.arima(c(model$x, x[i]), allowdrift = TRUE, allowmean = TRUE)       
        i <- i + 1
      }
    }
    else {
      pred <- forecast(obj$model, h = steps_ahead)$mean
    }
  }
  return(pred)
}


# setup for sliding window

do_train <- function(obj, x, y = NULL) {
  UseMethod("do_train")
}

do_predict <- function(obj, x) {
  UseMethod("do_predict")
}

#class tsreg_sw

tsreg_sw <- function(preprocess, input_size) {
  obj <- tsreg()
  
  obj$preprocess <- preprocess
  obj$input_size <- input_size
  
  class(obj) <- append("tsreg_sw", class(obj))  
  return(obj)
}

ts_as_matrix <- function(data, input_size) {
  result <- data[,(ncol(data)-input_size+1):ncol(data)]
  colnames(result) <- colnames(data)[(ncol(data)-input_size+1):ncol(data)]
  return(result)
}

train.tsreg_sw <- function(obj, x, y) {
  obj <- train.tsreg(obj, x, y)
  
  obj$preprocess <- prepare(obj$preprocess, x)
  
  x <- action(obj$preprocess, x)
  
  y <- action(obj$preprocess, x, y)
  
  obj <- do_train(obj, ts_as_matrix(x, obj$input_size), y)
  
  obj <- register_log(obj, obj$msg)    
  return(obj)
}

predict.tsreg_sw <- function(obj, x, steps_ahead=1) {
  if (steps_ahead == 1) {
    x <- action(obj$preprocess, x)
    y <- do_predict(obj, ts_as_matrix(x, obj$input_size))
    y <- deaction(obj$preprocess, x, y)
    return(y)
  }
  else {
    prediction <- NULL
    x <- as.data.frame(x)
    cnames <- colnames(x)
    x <- x[1,]
    for (i in 1:steps_ahead) {
      colnames(x) <- cnames
      x <- action(obj$preprocess, x)
      y <- do_predict(obj, ts_as_matrix(x, obj$input_size))
      x <- deaction(obj$preprocess, x)
      y <- deaction(obj$preprocess, x, y)
      x <- cbind(x[,2:ncol(x)], y)
      prediction <- c(prediction, y)
    }
    return(prediction)
  }
  return(prediction)
}

do_predict.tsreg_sw <- function(obj, x) {
  prediction <- predict(obj$model, x)  
  return(prediction)
}

#class tsreg_rf

tsreg_rf <- function(preprocess, input_size, mtry = NULL, ntree = seq(5, 50, 5)) {
  obj <- tsreg_sw(preprocess, input_size)
  
  if (is.null(mtry))
    mtry <- ceiling(c(1,1.5,2)*input_size/3)
  obj$mtry <- mtry
  obj$ntree <- ntree
  
  class(obj) <- append("tsreg_rf", class(obj))    
  return(obj)
}

do_train.tsreg_rf <- function(obj, x, y) {
  loadlibrary("randomForest")

  ranges <- list(mtry=obj$mtry, ntree=obj$ntree)
  obj$model <- tune.tsreg(x = x, y = y, ranges = ranges, train.func = randomForest)
  
  params <- attr(obj$model, "params") 
  obj$msg <- sprintf("mtry=%d,ntree=%d", params$mtry, params$ntree)
  return(obj)
}


#class tsreg_mlp

tsreg_mlp <- function(preprocess, input_size, size=NULL, decay=seq(0, 1, 0.0335), maxit=1000) {
  obj <- tsreg_sw(preprocess, input_size)
  
  if (is.null(size))
    size <- ceiling(input_size/3)
  obj$size <- size
  obj$decay <- decay
  obj$maxit <- maxit
  
  class(obj) <- append("tsreg_mlp", class(obj))  
  return(obj)
}

do_train.tsreg_mlp <- function(obj, x, y) {
  loadlibrary("nnet")  
  ranges <- list(size = obj$size, decay=obj$decay, maxit = obj$maxit, linout=TRUE, trace = FALSE)
  obj$model <- tune.tsreg(x = x, y = y, ranges = ranges, train.func = nnet)
  
  params <- attr(obj$model, "params") 
  obj$msg <- sprintf("size=%d,decay=%.2f", params$size, params$decay)
  return(obj)
}


#class tsreg_svm

tsreg_svm <- function(preprocess, input_size, epsilon=seq(0,1,0.2), cost=seq(20,100,20), kernel="radial") {
  obj <- tsreg_sw(preprocess, input_size)
  
  obj$kernel <- kernel
  obj$epsilon <- epsilon
  obj$cost <- cost
  
  class(obj) <- append("tsreg_svm", class(obj))  
  return(obj)
}

do_train.tsreg_svm <- function(obj, x, y) {
  loadlibrary("e1071")
  
  ranges <- list(epsilon=obj$epsilon, cost=obj$cost, kernel=obj$kernel)
  obj$model <- tune.tsreg(x = x, y = y, ranges = ranges, train.func = svm)
  
  params <- attr(obj$model, "params") 
  obj$msg <- sprintf("epsilon=%.1f,cost=%.3f", params$epsilon, params$cost)
  return(obj)
}

#class tsreg_elm

tsreg_elm <- function(preprocess, input_size, nhid=3:8, actfun = c('sig', 'radbas', 'tribas', 'relu', 'purelin')) {
  obj <- tsreg_sw(preprocess, input_size)
  if (is.null(nhid))
    nhid <- input_size/3
  obj$nhid <- nhid
  obj$actfun <- actfun
  
  class(obj) <- append("tsreg_elm", class(obj))    
  return(obj)
}

ts_train.tsreg_elm <- function(x, y, nhid, actfun, ...) {
  model <- elm_train(x, y, nhid = nhid, actfun = as.character(actfun), init_weights = "uniform_positive", bias = FALSE, verbose = FALSE)  
  return(model)
}

do_train.tsreg_elm <- function(obj, x, y) {
  loadlibrary("elmNNRcpp")
  
  ranges <- list(nhid = obj$nhid, actfun = "purelin", init_weights = "uniform_positive", bias = FALSE, verbose = FALSE)
  obj$model <- tune.tsreg(x = x, y = y, ranges = ranges, train.func = ts_train.tsreg_elm, pred.fun = elm_predict)
  
  params <- attr(obj$model, "params") 
  obj$msg <- sprintf("nhid=%d,actfun=%s", params$nhid, params$actfun)
  return(obj)
}

do_predict.tsreg_elm <- function(obj, x) {
  if (is.data.frame(x))
    x <- as.matrix(x)
  prediction <- elm_predict(obj$model, x)
  return(prediction)
}


#class tsreg_cnn

tsreg_cnn <- function(preprocess, input_size, neurons=c(3,5,10,16,32), epochs = 200) {
  obj <- tsreg_sw(preprocess, input_size)
  obj$neurons <- neurons
  obj$epochs <- epochs
  
  class(obj) <- append("tsreg_cnn", class(obj))    
  return(obj)
}

do_train.tsreg_cnn <- function(obj, x, y) {
  internal_train.tsreg_cnn <- function(x, y, neurons, epochs, ...) {
    x <- adjust.data.frame(x)
    xy <- x
    xy$t0 <- y
    
    spec <- feature_spec(xy, t0 ~ . ) %>% 
      step_numeric_column(all_numeric(), normalizer_fn = scaler_standard()) %>% 
      fit()
    
    input <- layer_input_from_dataset(x)
    
    output <- input %>% 
      layer_dense_features(dense_features(spec)) %>% 
      layer_dense(units = neurons, activation = "relu") %>% 
      layer_dense(units = neurons, activation = "relu") %>%
      layer_dense(units = 1) 
    
    model <- keras_model(input, output)
    
    model %>% 
      compile(loss = "mse", optimizer = optimizer_rmsprop(), 
              metrics = list("mean_absolute_error"))
    
    history <- model %>% fit(
      x = x,
      y = y,
      epochs = epochs,
      validation_split = 0.2,
      verbose = 0
    )  
    
    return (model)
  }  
  
  internal_predict.tsreg_cnn <- function(model, x) {
    x <- adjust.data.frame(x)  
    return(predict(model, x))  
  }
  
  loadlibrary("dplyr")
  loadlibrary("tfdatasets")
  loadlibrary("tensorflow")
  loadlibrary("keras")  
  
  tf$get_logger()$setLevel('ERROR')
  ranges <- list(neurons = obj$neurons, epochs = obj$epochs)
  obj$model <- tune.tsreg(x = x, y = y, ranges = ranges, train.func = internal_train.tsreg_cnn, pred.fun = internal_predict.tsreg_cnn)
  tf$get_logger()$setLevel('WARNING')
  
  params <- attr(obj$model, "params") 
  obj$msg <- sprintf("neurons=%d,epochs=%d", params$neurons, params$epochs)
  return(obj) 
}

do_predict.tsreg_cnn <- function(obj, x) {
  loadlibrary("dplyr")
  loadlibrary("tensorflow")
  loadlibrary("keras")
  
  x <- adjust.data.frame(x)  
  return(predict(obj$model, x))  
}

#class tsreg_lstm

tsreg_lstm <- function(preprocess, input_size, neurons=c(3,5,10,16,32), epochs = 200) {
  obj <- tsreg_sw(preprocess, input_size)
  
  obj$neurons <- neurons
  obj$epochs <- epochs
  
  class(obj) <- append("tsreg_lstm", class(obj))    
  return(obj)
}

do_train.tsreg_lstm <- function(obj, x, y) {
  internal_train.tsreg_lstm <- function(x, y, neurons, epochs, ...) {
    batch.size <- 1
    size <- ncol(x)
    
    x <- array(as.vector(x), dim=(c(dim(x),1)))
    
    model <- keras_model_sequential()
    model %>%
      layer_lstm(units = neurons,
                 input_shape = c(size, 1),
                 batch_size = batch.size,
                 return_sequences = TRUE,
                 stateful = TRUE) %>%
      layer_dropout(rate = 0.5) %>%
      layer_lstm(units = neurons,
                 return_sequences = FALSE,
                 stateful = TRUE) %>%
      layer_dropout(rate = 0.5) %>%
      layer_dense(units = 1)
    
    model %>%
      compile(loss = 'mae', optimizer = 'adam')
    
    history <- model %>% fit(x = x,
                             y = y,
                             batch_size = batch.size,
                             epochs = epochs,
                             verbose = 0,
                             shuffle = FALSE)
    
    model %>% reset_states()
    
    return (model)
  }
  
  internal_predict.tsreg_lstm <- function(model, x) {
    dat <- array(as.vector(as.matrix(x)), dim=(c(dim(x),1)))
    batch.size <- 1
    prediction <- model %>% predict(dat, batch_size = batch.size) %>% .[,1]
    return(prediction)  
  }
  
  loadlibrary("dplyr")
  loadlibrary("tfdatasets")
  loadlibrary("tensorflow")
  loadlibrary("keras")  
  
  tf$get_logger()$setLevel('ERROR')
  ranges <- list(neurons = obj$neurons, epochs = obj$epochs)
  obj$model <- tune.tsreg(x = x, y = y, ranges = ranges, train.func = internal_train.tsreg_lstm, pred.fun = internal_predict.tsreg_lstm)
  tf$get_logger()$setLevel('WARNING')
  
  params <- attr(obj$model, "params") 
  obj$msg <- sprintf("neurons=%d,epochs=%d", params$neurons, params$epochs)
  return(obj) 
}


do_predict.tsreg_lstm <- function(obj, x) {
  loadlibrary("dplyr")
  loadlibrary("tensorflow")
  loadlibrary("keras")  
  
  dat <- array(as.vector(as.matrix(x)), dim=(c(dim(x),1)))
  batch.size <- 1
  prediction <- obj$model %>% predict(dat, batch_size = batch.size) %>% .[,1]
  return(prediction)  
}

# utility functions


tsreg.reproduce <- TRUE
tsreg.check_reproduce <- function() {
  if (tsreg.reproduce)
    set.seed(1)
}

tune.tsreg <- function (x, y, ranges, folds=3, train.func, pred.fun = predict) {
  ranges <- expand.grid(ranges)
  n <- nrow(ranges)
  errors <- rep(0,n)
  data <- data.frame(i = 1:nrow(x), idx = 1:nrow(x))
  folds <- k_fold(sample_random(), data, folds)
  
  i <- 1
  if (n > 1) {
    for (i in 1:n) {
      for (j in 1:length(folds)) {
        tsreg.check_reproduce()
        tt <- train_test_from_folds(folds, j)
        params <- append(list(x = x[tt$train$i,], y = y[tt$train$i,]), as.list(ranges[i,]))
        model <- do.call(train.func, params)
        prediction <- pred.fun(model, x[tt$test$i,]) 
        errors[i] <- errors[i] + evaluation.tsreg(y[tt$test$i,], prediction)$mse 
      }
    }
    i <- which.min(errors)
  }
  params <- append(list(x = x, y = y), as.list(ranges[i,]))
  tsreg.check_reproduce()
  model <- do.call(train.func, params)
  attr(model, "params") <- as.list(ranges[i,])
  return(model)
}

# regression_evaluation

MSE.tsreg <- function (actual, prediction) 
{
  if (length(actual) != length(prediction)) 
    stop("actual and prediction have different lengths")
  n <- length(actual)
  res <- mean((actual - prediction)^2)
  res
}

sMAPE.tsreg <- function (actual, prediction) 
{
  if (length(actual) != length(prediction)) 
    stop("actual and prediction have different lengths")
  n <- length(actual)
  res <- (1/n) * sum(abs(actual - prediction)/((abs(actual) + 
                                                  abs(prediction))/2))
  res
}

evaluation.tsreg <- function(values, prediction) {
  obj <- list(values=values, prediction=prediction)

  obj$smape <- sMAPE.tsreg(values, prediction)  
  obj$mse <- MSE.tsreg(values, prediction)  
  
  obj$metrics <- data.frame(mse=obj$mse, smape=obj$smape)
  
  attr(obj, "class") <- "evaluation.tsreg"  
  return(obj)
}

plot.tsreg <- function(obj, y, yadj, ypre, xlabels=NULL) {
  loadlibrary("TSPred")
  prepname <- ""
  if (!is.null(obj$preprocess))
    prepname <- sprintf("-%s", class(obj$preprocess)[1])
  modelname <- sprintf("%s%s", class(obj)[1], prepname)
  ntrain <- length(yadj)
  smape_train <- sMAPE.tsreg(y[1:ntrain], yadj)*100
  smape_test <- sMAPE.tsreg(y[(ntrain+1):(ntrain+length(ypre))], ypre)*100
  par(xpd=TRUE)      
  if(is.null(xlabels))
    xlabels <- 1:length(y)
  plot(xlabels, y, main = modelname, xlab = sprintf("time [smape train=%.2f%%], [smape test=%.2f%%]", smape_train, smape_test), ylab="value")
  lines(xlabels[1:ntrain], yadj, col="blue")  
  lines(xlabels[ntrain:(ntrain+length(ypre))], c(yadj[length(yadj)],ypre), col="green")  
  par(xpd=FALSE)      
}