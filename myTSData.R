# version 1.0
#source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/mySample.R")

ts_data <- function(data, sw=0) {
  data <- as.matrix(data)
  if(ncol(data)==1)
    colnames(data) <- "t0"
  else 
    sw <- ncol(data)
  obj <- rel_transform(data)
  obj$sw <- sw
  
  class(obj) <- append("ts_data", class(obj))    
  return(obj)
}

prepare.ts_data <- function(obj) {
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
    col <- paste("t",c((sw_size-1):0), sep="")
    colnames(sw) <- col
    return(sw)  
  }
  data <- obj$data
  if(obj$sw > 1) {
    obj$data <- ts_sw(obj$data[,ncol(obj$data)], obj$sw)  
  }
  return(obj)
}

action.ts_data <- function(obj) {
  if (ncol(obj$data)>1)
    return(obj$data)
  else
    return(obj$data[,1])
}

#ts_projection
ts_projection <- function(obj) {
  if (ncol(obj$data)==1) {
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

