# version 1.0
#source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/mySample.R")

ts_data <- function(data, sw=0) {
  ts_sw <- function(x, sw_size) {
    ts_lag <- function(x, k) 
    {
      c(rep(NA, k), x)[1 : length(x)] 
    }
    n <- length(x)-sw_size+1
    sw <- NULL
    for(c in (sw_size-1):0){
      t  <- ts_lag(x,c)
      t <- t[sw_size:length(t)]
      sw <- cbind(sw,t,deparse.level = 0)
    }
    return(sw)  
  }
  if (sw <= 1) {
    sw <- 0
    data <- as.matrix(data)
  }
  else 
    data <- ts_sw(as.matrix(data), sw)  
  col <- paste("t",(ncol(data)-1):0, sep="")
  colnames(data) <- col
  obj <- list(data=data, sw=sw)
  
  class(obj) <- "ts_data"    
  return(obj)
}

#ts_projection
ts_projection <- function(obj) {
  if (ncol(obj$data) == 1) {
    input <- obj$data
    output <- NULL
  }
  else {
    input <- obj$data[,1:ncol(obj$data)-1]
    output <- obj$data[,ncol(obj$data)]
  }
  obj <- list(input = input, output = output)
  attr(obj, "class") <- "ts_projection"  
  return(obj)
}

