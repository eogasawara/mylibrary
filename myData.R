# version 1.0
# debug: setwd("C:/Users/eduar/OneDrive/Git/mylibrary")
myrepos <- "http://cran.fiocruz.br"


setrepos <- function(repos=repos) {
  myrepos <- repos 
}

loadlibrary <- function(x) 
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x, repos=myrepos, dep=TRUE)
    require(x)
  }
}

dal_data <- function(data) {
  return(obj)
}

#converter ts_data para matrix
#definir método para não perder a classe e os atributos
#https://stackoverflow.com/questions/7532845/matrix-losing-class-attribute-in-r
#ts_data
ts_data <- function(data, sw=0) {
  if (sw <= 1) {
    sw <- 0
    data <- as.matrix(data)
  }
  else 
    data <- ts_sw(as.matrix(data), sw)  
  col <- paste("t",(ncol(data)-1):0, sep="")
  colnames(data) <- col
  
  obj <- list(data=data, sw=sw)
  attr(obj, "class") <- "ts_data"  
  return(obj)
}

#ts_sample

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


# general functions
adjust.matrix <- function(data) {
  if(!is.matrix(data)) {
    return(as.matrix(data))
  }
  else
    return(data)
}

adjust.data.frame <- function(data) {
  if(!is.data.frame(data)) {
    return(as.data.frame(data))
  }
  else
    return(data)
}

ts_sw <- function(x, sw) {
  ts_lag <- function(x, k) 
  {
    c(rep(NA, k), x)[1 : length(x)] 
  }
  n <- length(x)-sw+1
  sw <- NULL
  for(c in (sw-1):0){
    t  <- ts_lag(x,c)
    t <- t[sw:length(t)]
    sw <- cbind(sw,t,deparse.level = 0)
  }
  return(sw)  
}
