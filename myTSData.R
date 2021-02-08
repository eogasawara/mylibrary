# version 1.0
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myRelation.R")
#source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/mySample.R")

ts_data <- function(data, sw=NULL) {
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
  if(!is.null(obj$sw)) {
    obj$data <- ts_sw(as.matrix(obj$data), obj$sw)  
  }
  return(obj)
}

action.ts_data <- function(obj) {
  return(obj$data)
}

length.ts_data <- function(obj) {
  if (obj$sw > 0)
    return(nrow(obj$data))
  else
    return(length(obj$data))
}

train_test.ts_data <- function(obj, test_size=NULL, offset=0) {
  offset <- length(obj)-test_size-offset
  if (sw_size == 0) {
    obj$train <- obj$data[1:offset]
    obj$test <- obj$data[(offset+1):(offset+test_size)]
  }
  else {
    obj$train <- obj$data[1:offset,]
    obj$test <- obj$data[(offset+1):(offset+test_size),]    
  }
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