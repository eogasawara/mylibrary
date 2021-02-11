# version 1.0
#source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myRelation.R")

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
ts_sample <- function(ts, test_size=1, offset=0) {
  train <- ts
  test <- ts
  
  offset <- nrow(ts$data) - test_size - offset
  train$data <- ts$data[1:offset, ]
  test$data <- ts$data[(offset+1):(offset+test_size),]
  if (ncol(ts$data) == 1) {
    train$data <- adjust.matrix(train$data)
    colnames(train$data) <- colnames(ts$data)
    test$data <- adjust.matrix(test$data)
    colnames(test$data) <- colnames(ts$data)
  }
  
  samp <- list(train = train, test = test)
  attr(samp, "class") <- "ts_sample"  
  return(samp)
}


#ts_projection
ts_projection <- function(ts) {
  input <- ts
  output <- ts
  
  if (ncol(ts$data) == 1) {
    input$data <- ts$data
    output <- NULL
  }
  else {
    input$data <- ts$data[,1:ncol(ts$data)-1]
    output$data <- ts$data[,ncol(ts$data)]
  }
  
  proj <- list(input = input, output = output)
  attr(proj, "class") <- "ts_projection"  
  return(proj)
}
