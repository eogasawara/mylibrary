loadlibrary <- function(x, repos='http://cran.fiocruz.br') 
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x, repos=repos, dep=TRUE)
    require(x)
  }
}

loadlibrary("nnet")
loadlibrary("smooth")
loadlibrary("Mcomp")
loadlibrary("ModelMetrics")
loadlibrary("randomForest")
loadlibrary("RSNNS")
loadlibrary("kernlab")
loadlibrary("elmNNRcpp")
loadlibrary("e1071")
loadlibrary("nnfor")
loadlibrary("TSPred")

timeseries_debug <- FALSE

ts.sw <- function(x,k)
{
  ts.lagPad <- function(x, k) 
  {
    c(rep(NA, k), x)[1 : length(x)] 
  }
  
  n <- length(x)-k+1
  sw <- NULL
  for(c in (k-1):0){
    t  <- ts.lagPad(x,c)
    sw <- cbind(sw,t,deparse.level = 0)
  }
  col <- paste("t",c((k-1):0), sep="")
  rownames(sw) <- NULL
  colnames(sw) <- col
  return (sw)
}

ts.as.matrix <- function(sw, size) {
  sw <- data.frame(sw)
  sw <- sw[, (ncol(sw)-size+1):ncol(sw)]
  sw <- as.matrix(sw)
  return(sw)
}

ts.swProject <- function(sw) 
{
  input <- sw[,1:ncol(sw)-1]
  output <- sw[,ncol(sw)]
  return (list(input=input, output=output))
} 

ts.emean <- function(x) {
  n <- length(x)
  y <- rep(0,n)
  alfa = 1 - 2.0 / (n + 1);
  for (i in 0:(n-1)) {
    y[n-i] <- alfa^i
  }
  m <- sum(y * x)/sum(y)
  return(m)
}

ts.mse <- function(x, y) {
  delta <- (x - y)^2
  return(mean(delta))
}

ts.an.outliers.boxplot <- function(x, k, alpha=1.5) {
  ts.sw <- function(x,k)
  {
    ts.lagPad <- function(x, k) 
    {
      c(rep(NA, k), x)[1 : length(x)] 
    }
    
    n <- length(x)-k+1
    sw <- NULL
    for(c in (k-1):0){
      t  <- ts.lagPad(x,c)
      sw <- cbind(sw,t,deparse.level = 0)
    }
    col <- paste("t",c((k-1):0), sep="")
    rownames(sw) <- NULL
    colnames(sw) <- col
    return (sw)
  }
  
  outliers.boxplot <- function(data, alpha)
  {
    org = nrow(data)
    cond <- rep(FALSE, org)
    if (org >= 30) {
      i = ncol(data)
      q = quantile(data[,i], na.rm=TRUE)
      IQR = q[4] - q[2]
      lq1 = q[2] - alpha*IQR
      hq3 = q[4] + alpha*IQR
      cond = data[,i] < lq1 | data[,i] > hq3
    }
    return (cond)
  }
  
  sx <- ts.sw(x,k)
  ma <- apply(sx, 1, mean)
  sxd <- sx - ma
  iF <- outliers.boxplot(sxd,alpha)
  
  
  sx <- ts.sw(rev(x),k)
  ma <- apply(sx, 1, mean)
  sxd <- sx - ma
  iB <- outliers.boxplot(sxd,alpha)
  iB <- rev(iB)
  
  i <- iF & iB
  i[1:k] <- iB[1:k]
  i[(length(x)-k+1):length(x)] <- iF[(length(x)-k+1):length(x)]
  
  return(i)
}

ts.outliers.boxplot <- function(x, alpha = 1.5)
{
  if (length(x) >= 30) {
    q = quantile(x)
    IQR = q[4] - q[2]
    lq1 = q[2] - alpha*IQR
    hq3 = q[4] + alpha*IQR
    cond = x >= lq1 & x <= hq3
    x = x[cond]
  }
  return (x)
}

ts.sw.outliers.boxplot <- function(data, alpha = 1.5)
{
  org = nrow(data)
  if (org >= 30) {
    q = as.data.frame(lapply(data, quantile))
    n = ncol(data)
    for (i in 1:n)
    {
      IQR = q[4,i] - q[2,i]
      lq1 = q[2,i] - alpha*IQR
      hq3 = q[4,i] + alpha*IQR
      cond = data[,i] >= lq1 & data[,i] <= hq3
      data = data[cond,]
    }
  }
  return (data)
}

ts.gmm.norm.minmax <- function(serie, scale=1, offset=0, rescale=TRUE) 
{
  ts.minmax_n <- function(data, par)
  {
    return (par$scale*(data-par$gmin)/(par$gmax-par$gmin) + par$offset)
  }
  
  ts.minmax_d <- function(data, par)
  {
    return ((data - par$offset) * (par$gmax-par$gmin) / par$scale + par$gmin)
  }
  
  io <- ts.swProject(serie)
  
  par <- list(gmin = min(serie), gmax = max(serie))
  
  if (rescale) {
    swi_min <- apply(io$input, 1, min)
    swi_max <- apply(io$input, 1, max)
    
    swio_min <- apply(serie, 1, min)
    swio_max <- apply(serie, 1, max)
    
    ratio <- (swi_max-swi_min)/(swio_max-swio_min)
    ratio <- ts.outliers.boxplot(ratio)
    ratio <- mean(ratio)
    
    w <- (par$gmax - par$gmin)/(2*ratio)
    c <- (par$gmax + par$gmin)/2
    par$gmax <- c + w
    par$gmin <- c - w
  }
  
  par$scale = scale
  par$offset = offset
  par$rescale <- rescale
  
  return(list(par = par, norm = ts.minmax_n, dnorm = ts.minmax_d))
}


ts.diff.norm.minmax <- function(serie, scale=1, offset=0, rescale=TRUE) 
{
  ts.minmax_n <- function(data, par)
  {
    return (par$scale*(data-par$gmin)/(par$gmax-par$gmin) + par$offset)
  }
  
  ts.minmax_d <- function(data, par)
  {
    return ((data - par$offset) * (par$gmax-par$gmin) / par$scale + par$gmin)
  }
  
  io <- ts.swProject(serie)
  
  if (rescale) {
    swi_min <- apply(io$input, 1, min)
    swi_max <- apply(io$input, 1, max)
    
    swio_min <- apply(serie, 1, min)
    swio_max <- apply(serie, 1, max)
    
    ratio <- (swi_max-swi_min)/(swio_max-swio_min)
    ratio <- ts.outliers.boxplot(ratio)
    ratio <- mean(ratio)
    
    offset <- offset + (1 - ratio) * scale / 2
    scale <- scale * ratio
  }
  
  par <- list(gmin = min(io$output), gmax = max(io$output))
  par$scale <- scale
  par$offset <- offset
  par$rescale <- rescale
  
  return(list(par = par, norm = ts.minmax_n, dnorm = ts.minmax_d))
}


ts.sw.norm.minmax <- function(serie, scale=1, offset=0, rescale=TRUE) 
{
  ts.minmax <- function(data, par) 
  {
    par$swi_min <- apply(data, 1, min)
    par$swi_max <- apply(data, 1, max)
    return(par)
  } 
  
  ts.minmax_n <- function(data, par)
  {
    return (par$scale*(data-par$swi_min)/(par$swi_max-par$swi_min) + par$offset)
  }
  
  ts.minmax_d <- function(data, par)
  {
    return ((data - par$offset) * (par$swi_max-par$swi_min) / (par$scale) + par$swi_min)
  }
  
  io <- ts.swProject(serie)
  
  if (rescale) {
    swi_min <- apply(io$input, 1, min)
    swi_max <- apply(io$input, 1, max)
    
    swio_min <- apply(serie, 1, min)
    swio_max <- apply(serie, 1, max)
    
    ratio <- (swi_max-swi_min)/(swio_max-swio_min)
    ratio <- ts.outliers.boxplot(ratio)
    ratio <- mean(ratio)
    
    offset <- offset + (1 - ratio) * scale / 2
    scale <- scale * ratio
  }
  
  par <- list()
  par$scale <- scale
  par$offset <- offset
  par$rescale <- rescale
  
  
  return(list(par = par, norm = ts.minmax_n, dnorm = ts.minmax_d, minmax = ts.minmax))
}

ts.an.norm.minmax <- function(serie, scale=1, offset=0, rescale=TRUE) 
{
  ts.minmax_n <- function(data, par)
  {
    return (par$scale*(data-par$swi_min)/(par$swi_max-par$swi_min) + par$offset)
  }
  
  ts.minmax_d <- function(data, par)
  {
    return ((data - par$offset) * (par$swi_max-par$swi_min) / (par$scale) + par$swi_min)
  }
  
  io <- ts.swProject(serie)
  
  par <- list(swi_min = min(io$input), swi_max = max(io$input))
  if (rescale) {
    ratio <- (par$swi_max-par$swi_min)/(max(serie)-min(serie))
    
    offset <- offset + (1 - ratio) * scale / 2
    scale <- scale * ratio
  }
  
  par$scale <- scale
  par$offset <- offset
  par$rescale <- rescale
  
  return(list(par = par, norm = ts.minmax_n, dnorm = ts.minmax_d))
}

ts.ml <- function(method, ...) {
  return(list(mlm=method, arguments = list(...)))
}

svm_ext <- function(X, Y, ...) {
  arguments = list(...)
  
  svm(x = X, y = Y, kernel="radial", type="eps-regression", scale=FALSE, epsilon=arguments$epsilon, cost=arguments$cost)
  return(mdl$best.model)
}

ts.arima_train <- function(data) {
  set.seed(1)
  
  mlmodel <- auto.arima(data, D=1, approximation = FALSE, allowdrift = TRUE, allowmean = TRUE) 
  pred <- mlmodel$residuals + data
  
  mse <- ts.mse(pred, data)
  smape <-  TSPred::sMAPE(data,pred)
  
  if (timeseries_debug) {
    plot(1:length(data), data, main = class(mlmodel)[1], xlab = "time", ylab="value")
    lines(1:length(pred), pred, col="green")  
  }
  
  return (list(mlmodel=mlmodel, fnorm=NA, train.mse=mse, train.smape=smape))
}

ts.arima_test <- function(model, value, predict_custom = predict) {
  pred <- forecast(model$mlmodel, h = length(value))
  
  mse <- -1
  smape <- -1
  
  value<-value[!is.na(value)]
  
  if (length(value) > 0) {
    mse <- ts.mse(pred$mean, value)
    smape <-  TSPred::sMAPE(value, pred$mean)
    
    if (timeseries_debug) {
      plot(1:length(value), value, main = class(model$mlmodel)[1], xlab = "time", ylab="value")
      lines(1:length(pred$mean), pred$mean, col="green")  
    }
  }
  return(list(prediction=pred$mean, value=value, mse=mse, smape=smape))
}

ts.dir_train <- function(data, mlearning) {
  set.seed(1)
  
  mlmodel <- do.call(function(...) mlearning$mlm(data, ...), mlearning$arguments)
  
  start <- length(data)-length(mlmodel$fitted)+1
  x <- data[start:length(data)]
  
  mse <- ts.mse(mlmodel$fitted, x)
  smape <-  TSPred::sMAPE(x,mlmodel$fitted)
  
  if (timeseries_debug) {
    plot(1:length(x), x, main = sprintf("%s-%d-%s", class(mlmodel)[1], max(mlmodel$lags),"dir"), xlab = "time", ylab="value")
    lines(1:length(mlmodel$fitted), mlmodel$fitted, col="green")  
  }
  
  return (list(mlmodel=mlmodel, fnorm=NA, train.mse=mse, train.smape=smape))
}

ts.dir_test <- function(model, value, predict_custom = predict) {
  pred <- forecast(model$mlmodel, h = length(value))
  
  mse <- -1
  smape <- -1
  
  value<-value[!is.na(value)]
  
  if (length(value) > 0) {
    mse <- ts.mse(pred$mean, value)
    smape <-  TSPred::sMAPE(value,pred$mean)
    if (timeseries_debug) {
      plot(1:length(value), value, main = sprintf("%s-%d-%s", class(model$mlmodel)[1], max(model$mlmodel$lags), "dir"), xlab = "time", ylab="value")
      lines(1:length(pred$mean), pred$mean, col="green")  
    }
  }
  return(list(prediction=pred$mean, value=value, mse=mse, smape=smape))
}

ts.diff_train <- function(data, size, mlearning, predict_custom = predict, mmscale, mmoffset, rescale) {
  t1 <- data[,ncol(data)-1]
  data <- data[,2:ncol(data)] - data[,1:(ncol(data)-1)]
  
  dataorg <- data
  data <- ts.sw.outliers.boxplot(data)
  
  fnorm <- ts.diff.norm.minmax(data, mmscale, mmoffset, rescale)
  
  data <- fnorm$norm(data, fnorm$par)
  
  io = ts.swProject(data)
  
  set.seed(1)
  
  mlmodel <- do.call(function(...) mlearning$mlm(ts.as.matrix(io$input, size), as.matrix(io$output), ...), mlearning$arguments)
  
  data <- fnorm$norm(dataorg, fnorm$par)
  io = ts.swProject(data)
  
  prediction <- predict_custom(mlmodel, ts.as.matrix(io$input, size))
  
  prediction <- as.vector(prediction)
  prediction <- fnorm$dnorm(prediction, fnorm$par)+t1
  output <- fnorm$dnorm(io$output, fnorm$par)+t1 
  
  train.mse = mse(prediction, output)
  train.smape = TSPred::sMAPE(output, prediction)
  
  if (timeseries_debug) {
    plot(1:length(output), output, main = sprintf("%s-%d-%s", class(mlmodel)[1], size, "diff"), xlab = "time", ylab="value")
    lines(1:length(prediction), prediction, col="green")  
  }
  
  return (list(mlmodel=mlmodel, fnorm=fnorm, train.mse=train.mse, train.smape=train.smape))
}

ts.diff_test <- function(model, fnorm, size, test, value, predict_custom = predict) {
  t1 <- test[,ncol(test)-1]
  test <- test[,2:ncol(test)] - test[,1:(ncol(test)-1)]
  
  test <- fnorm$norm(test, fnorm$par)
  test <- as.matrix(test)
  
  if ((nrow(test) == 1) && (nrow(test) != length(value))) {
    prediction <- NULL
    for (i in 1:length(value)) {
      pred <- as.vector(predict_custom(model$mlmodel, ts.as.matrix(test, size)))
      test[1,] <- c(test[1,2:ncol(test)], pred)
      prediction <- c(prediction, pred)
    }
    prediction <- fnorm$dnorm(prediction, fnorm$par)
    for (i in 1:length(prediction)) {
      prediction[i] <- prediction[i] + t1
      t1 <- prediction[i]
    }
  }
  else {
    prediction <- as.vector(predict_custom(model$mlmodel, ts.as.matrix(test, size)))
    prediction <- fnorm$dnorm(prediction, fnorm$par)
    prediction <- prediction + t1
  }
  
  mse <- -1
  smape <- -1
  
  value<-value[!is.na(value)]
  
  if (length(value) > 0) {
    mse <- ts.mse(prediction, value)
    smape <-  TSPred::sMAPE(value,prediction)
    
    if (timeseries_debug) {
      plot(1:length(value), value, main = sprintf("%s-%d-%s", class(model$mlmodel)[1], size, "diff"), xlab = "time", ylab="value")
      lines(1:length(prediction), prediction, col="green")  
    }
  }
  
  return(list(prediction=prediction, value=value, mse=mse, smape=smape))
}


ts.gmm_train <- function(data, size, mlearning, predict_custom = predict, mmscale, mmoffset, rescale) {
  dataorg <- data
  
  data <- ts.sw.outliers.boxplot(data)
  
  fnorm <- ts.gmm.norm.minmax(data, mmscale, mmoffset, rescale)
  
  data <- fnorm$norm(data, fnorm$par)
  
  io = ts.swProject(data)
  
  set.seed(1)
  
  mlmodel <- do.call(function(...) mlearning$mlm(ts.as.matrix(io$input, size), as.matrix(io$output), ...), mlearning$arguments)
  
  data <- fnorm$norm(dataorg, fnorm$par)
  
  io = ts.swProject(data)
  
  prediction <- predict_custom(mlmodel, ts.as.matrix(io$input, size))
  
  prediction <- as.vector(prediction)
  prediction <- fnorm$dnorm(prediction, fnorm$par)
  output <- fnorm$dnorm(io$output, fnorm$par)
  
  train.mse = mse(prediction, output)
  train.smape = TSPred::sMAPE(output, prediction)
  
  if (timeseries_debug) {
    plot(1:length(output), output, main = sprintf("%s-%d-%s", class(mlmodel)[1], size, "gmm"), xlab = "time", ylab="value")
    lines(1:length(prediction), prediction, col="green")  
  }
  
  return (list(mlmodel=mlmodel, fnorm=fnorm, train.mse=train.mse, train.smape=train.smape))
}

ts.gmm_test <- function(model, fnorm, size, test, value, predict_custom = predict) {
  test <- fnorm$norm(test, fnorm$par)
  test <- as.matrix(test)
  
  if ((nrow(test) == 1) && (nrow(test) != length(value))) {
    prediction <- NULL
    for (i in 1:length(value)) {
      pred <- as.vector(predict_custom(model$mlmodel, ts.as.matrix(test, size)))
      test[1,] <- c(test[1,2:ncol(test)], pred)
      prediction <- c(prediction, pred)
    }
  }
  else 
    prediction <- as.vector(predict_custom(model$mlmodel, ts.as.matrix(test, size)))
  prediction <- fnorm$dnorm(prediction, fnorm$par)
  
  mse <- -1
  smape <- -1
  
  value<-value[!is.na(value)]
  
  if (length(value) > 0) {
    mse <- ts.mse(prediction, value)
    smape <-  TSPred::sMAPE(value,prediction)
    if (timeseries_debug) {
      plot(1:length(value), value, main = sprintf("%s-%d-%s", class(model$mlmodel)[1], size, "gmm"), xlab = "time", ylab="value")
      lines(1:length(prediction), prediction, col="green")  
    }
  }
  
  return(list(prediction=prediction, value=value, mse=mse, smape=smape))
}

ts.swmm_train <- function(data, size, mlearning, predict_custom = predict, mmscale, mmoffset, rescale) {
  fnorm <- ts.sw.norm.minmax(data, mmscale, mmoffset, rescale)
  
  fnorm$par <- fnorm$minmax(ts.swProject(data)$input, fnorm$par)
  data <- fnorm$norm(data, fnorm$par)  
  
  io <- ts.swProject(data)
  
  set.seed(1)
  
  mlmodel <- do.call(function(...) mlearning$mlm(ts.as.matrix(io$input, size), as.matrix(io$output), ...), mlearning$arguments)
  
  prediction <- predict_custom(mlmodel, ts.as.matrix(io$input, size))
  
  prediction <- as.vector(prediction)
  
  prediction <- fnorm$dnorm(prediction, fnorm$par)
  output <- fnorm$dnorm(io$output, fnorm$par)
  
  train.mse = mse(prediction, output)
  train.smape = TSPred::sMAPE(output, prediction)
  
  if (timeseries_debug) {
    plot(1:length(output), output, main = sprintf("%s-%d-%s", class(mlmodel)[1], size,"swmm"), xlab = "time", ylab="value")
    lines(1:length(prediction), prediction, col="green")  
  }
  
  return (list(mlmodel=mlmodel, fnorm=fnorm, train.mse=train.mse, train.smape=train.smape))
}

ts.swmm_test <- function(model, fnorm, size, test, value, predict_custom = predict) {
  test <- as.matrix(test)
  if ((nrow(test) == 1) && (nrow(test) != length(value))) {
    prediction <- NULL
    for (i in 1:length(value)) {
      fnorm$par <- fnorm$minmax(test, fnorm$par)
      swtest <- fnorm$norm(test, fnorm$par)  
      
      pred <- as.vector(predict_custom(model$mlmodel, ts.as.matrix(swtest, size)))
      
      pred <- fnorm$dnorm(pred, fnorm$par)
      
      test[1,] <- c(test[1,2:ncol(test)], pred)
      prediction <- c(prediction, pred)
    }
  }
  else {
    fnorm$par <- fnorm$minmax(test, fnorm$par)
    test <- fnorm$norm(test, fnorm$par)  
    
    prediction <- as.vector(predict_custom(model$mlmodel, ts.as.matrix(test, size)))
    
    prediction <- fnorm$dnorm(prediction, fnorm$par)
  }
  
  
  mse <- -1
  smape <- -1
  
  value<-value[!is.na(value)]
  
  if (length(value) > 0) {
    mse <- ts.mse(prediction, value)
    smape <-  TSPred::sMAPE(value,prediction)
    
    if (timeseries_debug) {
      plot(1:length(value), value, main = sprintf("%s-%d-%s", class(model$mlmodel)[1], size, "swmm"), xlab = "time", ylab="value")
      lines(1:length(prediction), prediction, col="green")  
    }
  }
  
  return(list(prediction=prediction, value=value, mse=mse, smape=smape))
}

ts.an_train <- function(data, size, mlearning, predict_custom = predict, mmscale, mmoffset, rescale) {
  dataorg <- data
  
  an <- apply(ts.swProject(data)$input, 1, mean)
  data <- data/an
  data$an <- an
  data <- ts.sw.outliers.boxplot(data)
  an <- data$an
  data$an <- NULL
  
  fnorm <- ts.an.norm.minmax(data, mmscale, mmoffset, rescale)  
  
  data <- fnorm$norm(data, fnorm$par)
  io = ts.swProject(data)
  
  set.seed(1)
  
  mlmodel <- do.call(function(...) mlearning$mlm(ts.as.matrix(io$input, size), as.matrix(io$output), ...), mlearning$arguments)
  
  data <- dataorg
  an <- apply(ts.swProject(data)$input, 1, mean)
  data <- data/an
  
  data <- fnorm$norm(data, fnorm$par)
  io = ts.swProject(data)
  
  prediction <- predict_custom(mlmodel, ts.as.matrix(io$input, size))
  
  prediction <- as.vector(prediction)
  prediction <- fnorm$dnorm(prediction, fnorm$par)*an
  output <- fnorm$dnorm(io$output, fnorm$par)*an
  
  train.mse = mse(prediction, output)
  train.smape = TSPred::sMAPE(output, prediction)
  
  if (timeseries_debug) {
    plot(1:length(output), output, main = sprintf("%s-%d-%s", class(mlmodel)[1], size, "an"), xlab = "time", ylab="value")
    lines(1:length(prediction), prediction, col="green")  
  }
  
  return (list(mlmodel=mlmodel, fnorm = fnorm, train.mse=train.mse, train.smape=train.smape))
}

ts.an_test <- function(model, fnorm, size, test, value, predict_custom = predict) {
  test <- as.matrix(test)
  if ((nrow(test) == 1) && (nrow(test) != length(value))) {
    prediction <- NULL
    for (i in 1:length(value)) {
      an <- mean(test[1,])
      test_an <- test/an
      test_an <- fnorm$norm(test_an, fnorm$par)
      
      pred <- as.vector(predict_custom(model$mlmodel, ts.as.matrix(test_an, size)))
      pred <- fnorm$dnorm(pred, fnorm$par)*an
      
      test[1,] <- c(test[1,2:ncol(test)], pred)
      prediction <- c(prediction, pred)
    }
  }
  else {
    an <- apply(test, 1, mean)
    test <- test/an
    test <- fnorm$norm(test, fnorm$par)
    prediction <- as.vector(predict_custom(model$mlmodel, ts.as.matrix(test, size)))
    prediction <- fnorm$dnorm(prediction, fnorm$par)*an
  }
  
  mse <- -1
  smape <- -1
  
  value<-value[!is.na(value)]
  
  if (length(value) > 0) {
    mse <- ts.mse(prediction, value)
    smape <-  TSPred::sMAPE(value,prediction)
    
    if (timeseries_debug) {
      plot(1:length(value), value, main = sprintf("%s-%d-%s", class(model$mlmodel)[1], size, "an"), xlab = "time", ylab="value")
      lines(1:length(prediction), prediction, col="green")  
    }
  }
  
  return(list(prediction=prediction, value=value, mse=mse, smape=smape))
}


ts.ane_train <- function(data, size, mlearning, predict_custom = predict, mmscale, mmoffset, rescale) {
  dataorg <- data
  
  an <- apply(ts.swProject(data)$input, 1, ts.emean)
  data <- data/an
  data$an <- an
  data <- ts.sw.outliers.boxplot(data)
  an <- data$an
  data$an <- NULL
  
  fnorm <- ts.an.norm.minmax(data, mmscale, mmoffset, rescale)  
  
  data <- fnorm$norm(data, fnorm$par)
  
  io = ts.swProject(data)
  
  set.seed(1)
  
  mlmodel <- do.call(function(...) mlearning$mlm(ts.as.matrix(io$input, size), as.matrix(io$output), ...), mlearning$arguments)
  
  data <- dataorg
  an <- apply(ts.swProject(data)$input, 1, ts.emean)
  data <- data/an
  
  data <- fnorm$norm(data, fnorm$par)
  io = ts.swProject(data)
  
  prediction <- predict_custom(mlmodel, ts.as.matrix(io$input, size))
  
  prediction <- as.vector(prediction)
  prediction <- fnorm$dnorm(prediction, fnorm$par)*an
  output <- fnorm$dnorm(io$output, fnorm$par)*an
  
  train.mse = mse(prediction, output)
  train.smape = TSPred::sMAPE(output, prediction)
  
  if (timeseries_debug) {
    plot(1:length(output), output, main = sprintf("%s-%d-%s", class(mlmodel)[1], size, "ane"), xlab = "time", ylab="value")
    lines(1:length(prediction), prediction, col="green")  
  }
  
  return (list(mlmodel=mlmodel, fnorm = fnorm, train.mse=train.mse, train.smape=train.smape))
}

ts.ane_test <- function(model, fnorm, size, test, value, predict_custom = predict) {
  test <- as.matrix(test)
  if ((nrow(test) == 1) && (nrow(test) != length(value))) {
    prediction <- NULL
    for (i in 1:length(value)) {
      an <- ts.emean(test[1,])
      test_an <- test/an
      test_an <- fnorm$norm(test_an, fnorm$par)
      
      pred <- as.vector(predict_custom(model$mlmodel, ts.as.matrix(test_an, size)))
      pred <- fnorm$dnorm(pred, fnorm$par)*an
      
      test[1,] <- c(test[1,2:ncol(test)], pred)
      prediction <- c(prediction, pred)
    }
  }
  else {
    an <- apply(test, 1, ts.emean)
    test <- test/an
    test <- fnorm$norm(test, fnorm$par)
    prediction <- as.vector(predict_custom(model$mlmodel, ts.as.matrix(test, size)))
    prediction <- fnorm$dnorm(prediction, fnorm$par)*an
  }
  
  mse <- -1
  smape <- -1
  
  value<-value[!is.na(value)]
  
  if (length(value) > 0) {
    mse <- ts.mse(prediction, value)
    smape <-  TSPred::sMAPE(value,prediction)
    
    if (timeseries_debug) {
      plot(1:length(value), value, main = sprintf("%s-%d-%s", class(model$mlmodel)[1], size, "ane"), xlab = "time", ylab="value")
      lines(1:length(prediction), prediction, col="green")  
    }
  }
  
  return(list(prediction=prediction, value=value, mse=mse, smape=smape))
}

ts.ani_train <- function(data, size, mlearning, predict_custom = predict, mmscale, mmoffset, rescale) {
  dataorg <- data
  
  an <- apply(ts.swProject(data)$input, 1, mean)
  data <- data-an
  data$an <- an
  data <- ts.sw.outliers.boxplot(data)
  an <- data$an
  data$an <- NULL
  
  fnorm <- ts.an.norm.minmax(data, mmscale, mmoffset, rescale)  
  
  data <- fnorm$norm(data, fnorm$par)
  
  io = ts.swProject(data)
  
  set.seed(1)
  
  mlmodel <- do.call(function(...) mlearning$mlm(ts.as.matrix(io$input, size), as.matrix(io$output), ...), mlearning$arguments)
  
  data <- dataorg
  an <- apply(ts.swProject(data)$input, 1, mean)
  data <- data - an
  
  data <- fnorm$norm(data, fnorm$par)
  io = ts.swProject(data)
  
  prediction <- predict_custom(mlmodel, ts.as.matrix(io$input, size))
  
  prediction <- as.vector(prediction)
  prediction <- fnorm$dnorm(prediction, fnorm$par) + an
  output <- fnorm$dnorm(io$output, fnorm$par) + an
  
  train.mse = mse(prediction, output)
  train.smape = TSPred::sMAPE(output, prediction)
  
  if (timeseries_debug) {
    plot(1:length(output), output, main = sprintf("%s-%d-%s", class(mlmodel)[1], size, "ani"), xlab = "time", ylab="value")
    lines(1:length(prediction), prediction, col="green")  
  }
  
  return (list(mlmodel=mlmodel, fnorm = fnorm, train.mse=train.mse, train.smape=train.smape))
}

ts.ani_test <- function(model, fnorm, size, test, value, predict_custom = predict) {
  test <- as.matrix(test)
  if ((nrow(test) == 1) && (nrow(test) != length(value))) {
    prediction <- NULL
    for (i in 1:length(value)) {
      an <- mean(test[1,])
      test_an <- test - an
      test_an <- fnorm$norm(test_an, fnorm$par)
      
      pred <- as.vector(predict_custom(model$mlmodel, ts.as.matrix(test_an, size)))
      pred <- fnorm$dnorm(pred, fnorm$par) + an
      
      test[1,] <- c(test[1,2:ncol(test)], pred)
      prediction <- c(prediction, pred)
    }
  }
  else {
    an <- apply(test, 1, mean)
    test <- test - an
    test <- fnorm$norm(test, fnorm$par)
    prediction <- as.vector(predict_custom(model$mlmodel, ts.as.matrix(test, size)))
    prediction <- fnorm$dnorm(prediction, fnorm$par) + an
  }
  
  
  mse <- -1
  smape <- -1
  
  value<-value[!is.na(value)]
  
  if (length(value) > 0) {
    mse <- ts.mse(prediction, value)
    smape <-  TSPred::sMAPE(value,prediction)
    
    if (timeseries_debug) {
      plot(1:length(value), value, main = sprintf("%s-%d-%s", class(model$mlmodel)[1], size, "ani"), xlab = "time", ylab="value")
      lines(1:length(prediction), prediction, col="green")  
    }
  }
  
  return(list(prediction=prediction, value=value, mse=mse, smape=smape))
}

plot_series <- function(x, sw_size) {
  sm <- na.omit(ma(x,order=sw_size,centre=FALSE))
  plot(x)
  lines(sm,col="red")
}

train_test <- function(name, x, sw_size, offset, steps_ahead, input_size, neurons, mlearning, preprocessing) {
  mse_train <- -1
  smape_train <- -1
  mse_test <- -1
  smape_test <- -1
  train_length <- -1
  prediction <- NA
  test <- NA
  x <- x[1:(length(x)-offset+steps_ahead)]
  
  method <- NA
  model <- NA
  pred <- NA
  
  mlearning <- as.character(mlearning)
  preprocessing <- as.character(preprocessing)
  
  predict_custom = predict
  if ((mlearning == "arima") || (mlearning == "emlp") || (mlearning == "eelm")){ 
    train <- ts(x[1:(length(x)-steps_ahead)], start = 1, end=length(x)-steps_ahead, freq = 1)
    train_length <- length(train)
    test.value <- x[(length(x)-steps_ahead+1):length(x)]
    
    if (train_length > input_size + 2) {
      if (mlearning == "arima") {
        sw_size <- 0
        input_size <- 0
        neurons <- 0
        model <- ts.arima_train(data = train)
        pred <- ts.arima_test(model, test.value, predict_custom=forecast)
      }
      else {
        d <- 0
        if (preprocessing == "diff")
          d <- 1
        if(mlearning == "emlp") {
          method <- ts.ml(nnfor::mlp, lags=1:input_size, sel.lag=rep(FALSE, input_size), hd = neurons, difforder=d)
          predict_custom <- forecast
        }
        else if(mlearning == "eelm") {
          method <- ts.ml(nnfor::elm, lags=1:input_size, sel.lag=rep(FALSE, input_size), hd = neurons, difforder=d)
          predict_custom <- forecast
        }
        sw_size <- 0
        model <- ts.dir_train(data = train, mlearning = method)
        pred <- ts.dir_test(model, test.value, predict_custom)
      }
    }
  }
  else {
    mmscale <- 1
    mmoffset <- 0
    swx <- data.frame(ts.sw(x, sw_size))
    
    n <- nrow(swx)-steps_ahead
    train <- na.omit(swx[1:n,])
    
    test <- swx[(n+1):nrow(swx),]
    test.value = test$t0
    test$t0 <- NULL
    
    if (steps_ahead > 1)
      test <- test[1,]
    
    train_length <- nrow(train)
    
    if (train_length > 2) {
      if(mlearning == "nnet")
        method = ts.ml(nnet, size=neurons, maxit=5000, trace=FALSE)
      else if(mlearning == "mlp")
        method = ts.ml(RSNNS::mlp, size=neurons, learnFuncParams=c(0.1), maxit=5000)
      else if(mlearning == "rf")
        method = ts.ml(randomForest, ntree=neurons)
      else if(mlearning == "svm") {
        #method = ts.ml(svm_ext, scale = FALSE, epsilon=neurons*10/100, cost=neurons*10)
        method = ts.ml(svm, kernel="radial", type="eps-regression", scale = FALSE, epsilon=neurons*10/100, cost=neurons*10)
        mmscale <- 2
        mmoffset <- -1
      }
      else if(mlearning == "elm") {
        method = ts.ml(elm_train, nhid = neurons, actfun = 'purelin', init_weights = "uniform_positive", bias = FALSE, verbose = FALSE)
        predict_custom = elm_predict    
      }
      
      if ((preprocessing == "gmm") || (preprocessing == "gmmo")) {
        model = ts.gmm_train(data = train, size = input_size, mlearning = method, predict_custom, mmscale, mmoffset, rescale = preprocessing == "gmm")
        pred = ts.gmm_test(model, fnorm = model$fnorm, size = input_size, test, test.value, predict_custom)
      }
      else if ((preprocessing == "swmm") || (preprocessing == "swmmo")) {
        model = ts.swmm_train(data = train, size = input_size, mlearning = method, predict_custom, mmscale, mmoffset, rescale = preprocessing == "swmm")
        pred = ts.swmm_test(model, fnorm = model$fnorm, size = input_size, test, test.value, predict_custom)
      }
      else if ((preprocessing == "an") || (preprocessing == "ano")) {
        model = ts.an_train(data = train, size = input_size, mlearning = method, predict_custom, mmscale, mmoffset, rescale = preprocessing == "an")
        pred = ts.an_test(model, fnorm = model$fnorm, size = input_size, test, test.value, predict_custom)  
      }
      else if ((preprocessing == "ane") || (preprocessing == "aneo")) {
        model = ts.ane_train(data = train, size = input_size, mlearning = method, predict_custom, mmscale, mmoffset, rescale = preprocessing == "ane")
        pred = ts.ane_test(model, fnorm = model$fnorm, size = input_size, test, test.value, predict_custom)  
      }
      else if ((preprocessing == "ani") || (preprocessing == "anio"))  {
        model = ts.ani_train(data = train, size = input_size, mlearning = method, predict_custom, mmscale, mmoffset, rescale = preprocessing == "ani")
        pred = ts.ani_test(model, fnorm = model$fnorm, size = input_size, test, test.value, predict_custom)  
      }
      else if ((preprocessing == "diff") || (preprocessing == "diffo")) {
        model = ts.diff_train(data = train, size = input_size, mlearning = method, predict_custom, mmscale, mmoffset, rescale = preprocessing == "diff")
        pred = ts.diff_test(model, fnorm = model$fnorm, size = input_size, test, test.value, predict_custom)  
      }
    }
  }
  if (!is.na(model) && !is.na(pred)) {
    mse_train <- model$train.mse
    smape_train <- model$train.smape
    mse_test <- pred$mse
    smape_test <- pred$smape
    prediction <- pred$prediction

    return(list(name=name, train_length=train_length, sw_size=sw_size, offset=offset,
                steps_ahead=steps_ahead, input_size=input_size, neurons=neurons,
                mlearning=mlearning, preprocessing=preprocessing, 
                mse_train=mse_train, smape_train=smape_train, mse_test=mse_test, smape_test=smape_test, values=test.value, predictions=pred, model=model))
  }
}


tt_add_predictions <- function(dataset, global_horizon, value, pred) {
  for (i in 1:global_horizon) {
    name <- sprintf("v%d", i)
    dataset[,name] <- NA
    class(dataset[,name]) = "numeric"
  }
  
  for (i in 1:global_horizon) {
    name <- sprintf("p%d", i)
    dataset[,name] <- NA 
    class(dataset[,name]) = "numeric"
  }
  
  value <- as.vector(value)
  for (i in 1:length(value)) {
    name <- sprintf("v%d", i)
    dataset[,name] <- value[i]
  }
  
  pred <- as.vector(pred)
  for (i in 1:length(pred)) {
    name <- sprintf("p%d", i)
    dataset[,name] <- pred[i]
  }
  return(dataset)
}


