source("myNormalization.R")

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

#class ts_arima

ts_arima <- function() {
  obj <- tsregression()
  
  class(obj) <- append("ts_arima", class(obj))  
  return(obj)
}

prepare.ts_arima <- function(obj, x, y = NULL) {
  obj <- prepare.tsregression(obj, x, y)
  
  loadlibrary("forecast")  
  obj$mdl <- auto.arima(x, allowdrift = TRUE, allowmean = TRUE) 
  return(obj)
}

action.ts_arima <- function(obj, x, y = NULL, steps_ahead=NULL) {
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

#class ts_emlp_dir

tsreg_emlp_dir <- function(input_size, difforder=0, hd=NULL) {
  obj <- tsregression()
  
  obj$input_size <- input_size
  obj$difforder <- difforder
  if (is.null(hd))
    hd <- ceiling(input_size/3)
  obj$hd <- hd
  
  class(obj) <- append("tsreg_emlp_dir", class(obj)) 
  return(obj)  
}

prepare.tsreg_emlp_dir <- function(obj, x, y = NULL) {
  obj <- prepare.tsregression(obj, x, y)
  
  loadlibrary("nnfor")  
  x <- ts(x)
  obj$mdl <- nnfor::mlp(x, hd = obj$hd, lags=1:obj$input_size, sel.lag=rep(FALSE, obj$input_size), difforder=obj$difforder)
  return(obj)
}


action.tsreg_emlp_dir <- function(obj, x, y = NULL, steps_ahead=NULL) {
  loadlibrary("nnfor") 
  if (!is.null(x) && (length(obj$mdl$y) == length(x)) && (sum(obj$mdl$y-x) == 0)) {
    #get adjusted data
    prev <- length(x)-length(obj$mdl$fitted)    
    pred <- c(obj$mdl$y[1:prev], obj$mdl$fitted)
  }
  else {
    if (is.null(steps_ahead))
      steps_ahead <- length(x)
    pred <- forecast(obj$mdl, h = steps_ahead)
    pred <- pred$mean
  }
  return(pred)
}

#class tsreg_eelm_dir

tsreg_eelm_dir <- function(input_size, difforder=0, hd=NULL) {
  obj <- tsregression()

  obj$input_size <- input_size
  obj$difforder <- difforder
  if (is.null(hd))
    hd <- ceiling(input_size/3)
  obj$hd <- hd
  
  class(obj) <- append("tsreg_eelm_dir", class(obj))  
  return(obj)  
}

prepare.tsreg_eelm_dir <- function(obj, x, y = NULL) {
  obj <- prepare.tsregression(obj, x, y)
  
  loadlibrary("nnfor")  
  x <- ts(x)
  obj$mdl <- nnfor::elm(x, hd = obj$hd, lags=1:obj$input_size, sel.lag=rep(FALSE, obj$input_size), difforder=obj$difforder)
  return(obj)
}

action.tsreg_eelm_dir <- function(obj, x, y = NULL, steps_ahead=NULL) {
  loadlibrary("nnfor") 
  if (!is.null(x) && (length(obj$mdl$y) == length(x)) && (sum(obj$mdl$y-x) == 0)) {
    #get adjusted data
    prev <- length(x)-length(obj$mdl$fitted)    
    pred <- c(obj$mdl$y[1:prev], obj$mdl$fitted)
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

#class ts_nnet

ts_nnet <- function(preprocess, input_size, neurons=NULL, decay=seq(0, 1, 0.02), maxit=1000) {
  obj <- tsreg_sw(preprocess, input_size)
  
  if (is.null(neurons))
    neurons <- unique(1:ceiling(input_size/3))
  obj$neurons <- neurons
  obj$decay <- decay
  obj$maxit <- maxit
  
  class(obj) <- append("ts_nnet", class(obj))  
  return(obj)
}

ts_invoke_prepare.ts_nnet <- function(obj, x, y) {
  loadlibrary("e1071")
  loadlibrary("nnet")  
  tuned <- tune(nnet, x, y, maxit=obj$maxit, trace=FALSE, ranges=list(decay=obj$decay, size=obj$neurons, linout=TRUE))
  obj$mdl <- tuned$best.model
  return(obj)
}


#class ts_svm

ts_svm <- function(preprocess, input_size, epsilon=seq(0,1,0.1), cost=seq(5,100,5), kernel="radial") {
  obj <- tsreg_sw(preprocess, input_size)

  obj$kernel <- kernel
  obj$epsilon <- epsilon
  obj$cost <- cost
  
  class(obj) <- append("ts_svm", class(obj))  
  return(obj)
}

ts_invoke_prepare.ts_svm <- function(obj, x, y) {
  loadlibrary("e1071")
  
  tuned <- tune(svm, x, y, ranges=list(epsilon=seq(0,1,0.1), cost=1:100))
  obj$mdl <- tuned$best.model
  return(obj)
}

#class ts_rf

ts_rf <- function(preprocess, input_size, mtry = NULL, ntree = seq(50, 500, 50)) {
  obj <- tsreg_sw(preprocess, input_size)
  
  if (is.null(mtry))
    mtry <- unique(1:ceiling(input_size/3))
  obj$mtry <- mtry
  obj$ntree <- ntree
  
  class(obj) <- append("ts_rf", class(obj))    
  return(obj)
}

ts_invoke_prepare.ts_rf <- function(obj, x, y) {
  loadlibrary("e1071")
  loadlibrary("randomForest")
  
  tuned <- tune(randomForest, x, y, ranges=list(mtry=obj$mtry, ntree=obj$ntree))
  obj$mdl <- tuned$best.model 

  return(obj)
}


#class ts_elm

ts_elm <- function(preprocess, input_size, nhid=NULL) {
  obj <- tsreg_sw(preprocess, input_size)
  
  if (is.null(nhid))
    nhid <- unique(1:ceiling(input_size/3))
  obj$nhid <- nhid

  class(obj) <- append("ts_elm", class(obj))    
  return(obj)
}

ts_invoke_prepare.ts_elm <- function(obj, x, y) {
  loadlibrary("e1071")
  loadlibrary("elmNNRcpp")
  
  obj$mdl <- elm_train(x, y, nhid = max(obj$nhid), actfun = 'purelin', init_weights = "uniform_positive", bias = FALSE, verbose = FALSE)
  
  #tuned <- tune(randomForest, x, y, ranges=list(mtry=obj$mtry, ntree=obj$ntree))
  #obj$mdl <- tuned$best.model 
  
  return(obj)
}

ts_invoke_action.ts_elm <- function(obj, x) {
  prediction <- elm_predict(obj$mdl, x)
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