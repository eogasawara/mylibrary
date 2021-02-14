source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myData.R")
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myTransform.R")

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

#class ts_dir

tsreg_dir <- function() {
  obj <- tsregression()
  class(obj) <- append("tsreg_dir", class(obj))  
  return(obj)
}


action.tsreg_dir <- function(obj, x=NULL, steps_ahead=1) {
}


#class ts_arima

ts_arima <- function() {
  obj <- tsreg_dir()
  class(obj) <- append("ts_arima", class(obj))  
  return(obj)
}

prepare.ts_arima <- function(obj, x, y = NULL) {
  loadlibrary("forecast")  
  obj <- prepare.tsregression(obj, x, y)
  obj$mdl <- auto.arima(x, allowdrift = TRUE, allowmean = TRUE) 
  return(obj)
}

action.ts_arima <- function(obj, x, y = NULL, steps_ahead=NULL) {
  loadlibrary("forecast")  
  if (!is.null(x) && (length(obj$mdl$x) == length(x)) && (sum(obj$mdl$x-x) == 0))
    pred <- obj$mdl$x - obj$mdl$residuals    
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
  obj <- tsreg_dir()
  class(obj) <- append("tsreg_emlp_dir", class(obj))  
  obj$input_size <- input_size
  obj$difforder <- difforder
  if (is.null(hd))
    hd <- ceiling(input_size/3)
  obj$hd <- hd
  return(obj)  
}

prepare.tsreg_emlp_dir <- function(obj, x, y = NULL) {
  loadlibrary("nnfor")  
  obj <- prepare.tsregression(obj, x, y)
  x <- ts(x)
  obj$mdl <- nnfor::mlp(x, hd = obj$hd, lags=1:obj$input_size, sel.lag=rep(FALSE, obj$input_size), difforder=obj$difforder)
  return(obj)
}


action.tsreg_emlp_dir <- function(obj, x, y = NULL, steps_ahead=NULL) {
  loadlibrary("nnfor") 
  if (!is.null(x) && (length(obj$mdl$y) == length(x)) && (sum(obj$mdl$y-x) == 0)) {
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
  obj <- tsreg_dir()
  class(obj) <- append("tsreg_eelm_dir", class(obj))  
  obj$input_size <- input_size
  obj$difforder <- difforder
  if (is.null(hd))
    hd <- ceiling(input_size/3)
  obj$hd <- hd
  return(obj)  
}

prepare.tsreg_eelm_dir <- function(obj, x, y = NULL) {
  loadlibrary("nnfor")  
  obj <- prepare.tsregression(obj, x, y)
  x <- ts(x)
  obj$mdl <- nnfor::elm(x, hd = obj$hd, lags=1:obj$input_size, sel.lag=rep(FALSE, obj$input_size), difforder=obj$difforder)
  return(obj)
}

action.tsreg_eelm_dir <- function(obj, x, y = NULL, steps_ahead=NULL) {
  loadlibrary("nnfor") 
  if (!is.null(x) && (length(obj$mdl$y) == length(x)) && (sum(obj$mdl$y-x) == 0)) {
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
  UseMethod("ts_invoke_train")
}

ts_invoke_action <- function(obj, x, steps_ahead=1) {
  UseMethod("ts_invoke_train")
}

#class tsreg_sw

tsreg_sw <- function(preprocess, input_size) {
  obj <- tsreg_dir()
  class(obj) <- append("tsreg_sw", class(obj))  
  obj$preprocess <- preprocess
  obj$input_size <- input_size
  return(obj)
}

prepare.tsreg_sw <- function(obj, x, y) {
  set.seed(1)
  
  obj$preprocess <- ts_normalize(obj$preprocess, x)
  
  input <- ts_normalize(obj$preprocess, io$input)
  
  ouput <- ts_normalize(obj$preprocess, io$output, input$arguments)
  
  obj <- ts_invoke_train(obj, ts_as_matrix(input$x, obj$input_size), as.matrix(ouput$x), arguments)

  return(obj)
}

ts_predict.tsreg_sw <- function(obj, X, steps_ahead=1) {
  prediction <- NULL
  if (steps_ahead == 1) {
    input <- ts_normalize(obj$preprocess, X)
    pred <- ts_invoke_predict(obj, ts_as_matrix(input$x, obj$input_size))
    pred <- as.vector(pred)
    pred <- ts_denormalize(obj$preprocess, pred, input$arguments)
    prediction <- pred$x
  }
  else {
    X <- data.frame(X)[1,]
    for (i in 1:steps_ahead) {
      input <- ts_normalize(obj$preprocess, X)
      pred <- ts_invoke_predict(obj, ts_as_matrix(input$x, obj$input_size))
      pred <- as.vector(pred)
      pred <- ts_denormalize(obj$preprocess, pred, input$arguments)
      X[1,] <- c(X[1,2:ncol(X)], pred$x)
      prediction <- c(prediction, pred$x)
    }
  }
  return(prediction)
}

ts_test.tsreg_sw <- function(obj, test, steps_ahead=1) {
  io <- ts_sw_project(test)
  
  prediction <- ts_predict(obj, io$input, steps_ahead)
  
  obj$test_pred <- prediction
  obj$test_value <- io$output
  
  obj$test_smape <- TSPred::sMAPE(obj$test_value, obj$test_pred)  
  obj$test_mse <- TSPred::MSE(obj$test_value, obj$test_pred)  
  
  return(obj)
}

#class ts_nnet

ts_nnet <- function(preprocess, input_size) {
  obj <- tsreg_sw(preprocess, input_size)
  obj$maxit <- 50000
  class(obj) <- append("ts_nnet", class(obj))  
  return(obj)
}

ts_invoke_train.ts_nnet <- function(obj, X, Y, arguments = NULL) {
  tuned <- tune(nnet, X, Y, maxit=obj$maxit, trace=FALSE, ranges=list(decay=seq(0, 1, 0.01), size=(1:ncol(X))))
  obj$mdl <- tuned$best.model
  return(obj)
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