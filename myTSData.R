# version 1.0
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myRelation.R")
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/mySample.R")

ts_data <- function(data, sw=0) {
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
    obj$data <- ts_sw(as.matrix(obj$data), obj$sw)  
  }
  return(obj)
}

action.ts_data <- function(obj) {
  return(obj$data)
}

length.ts_data <- function(obj) {
  if (is.vector(obj$data))
    return(length(obj$data))
  else
    return(nrow(obj$data))
}

range_data <- function(obj, range) {
  UseMethod("range_data")
}

range_data.ts_data <- function(obj, range) {
  if (is.vector(obj$data))
    return(obj$data[range])
  else
    return(obj$data[range,])
}

train_test.ts_data <- function(obj, test_size=NULL, offset=0) {
  offset <- length(obj) - test_size - offset
  train <- range_data(obj, 1:offset)
  obj$test <- range_data(obj, (offset+1):(offset+test_size))
  obj$data <- train
  return(obj)
}

ts_prep_normalize <- function(obj) {
  UseMethod("ts_normalize")
}

ts_prep_normalize.default <- function(obj) {
  return(obj)
}

ts_normalize <- function(obj) {
  UseMethod("ts_normalize")
}

ts_normalize.default <- function(obj) {
  return(obj)
}

ts_denormalize <- function(obj) {
  UseMethod("ts_denormalize")
}

ts_denormalize.default <- function(obj) {
  return(obj)
}

sw_project <- function(obj) {
  UseMethod("sw_project")
}

sw_project.ts_data <- function(obj) 
{
  if (is.vector(obj$data)) {
    input <- obj$data
  }
  else {
    input <- obj$data[,1:ncol(obj$data)-1]
    output <- obj$data[,ncol(obj$data)]
  }
  obj$input <- input
  obj$output <- output
  return(obj)
} 

unused.ts_data <- function(obj) {
  ts_as_matrix <- function(sw, size) {
    sw <- data.frame(sw)
    sw <- sw[, (ncol(sw)-size+1):ncol(sw)]
    sw <- as.matrix(sw)
    return(sw)
  }
  return(obj) 
}