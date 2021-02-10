# version 1.0
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myRelation.R")

# class sample

data_sample <- function(data) {
  obj <- list(data=data)
  attr(obj, "class") <- "data_sample"  
  return(obj)
}

train_test <- function(obj, ...) {
  UseMethod("train_test")
}

train_test.default <- function(obj, ...) {
  return(obj)
}

k_fold <- function(obj, k) {
  UseMethod("k_fold")
}

k_fold.default <- function(obj, k) {
  return(obj)
}

k_fold.data_sample <- function(obj, k) {
  folds = list()
  p = 1.0 / k
  myobj <- obj
  while (k > 1) {
    obj = train_test(obj, p)
    obj$data = obj$test
    folds = append(folds, list(obj$train))
    k = k - 1
    p = 1.0 / k
  }
  folds = append(folds, list(obj$test))
  obj <- myobj
  obj$folds <- folds
  obj$data <- NULL
  return (obj)
}

# classe sample_random

sample_random <- function(data) {
  obj <- data_sample(data)
  class(obj) <- append("sample_random", class(obj))  
  return(obj)
}

train_test.sample_random <- function(obj, perc=0.8) {
  idx = base::sample(1:nrow(obj$data),as.integer(perc*nrow(obj$data)))
  obj$train = obj$data[idx,]
  obj$test = obj$data[-idx,]
  obj$data <- NULL
  return (obj)
}

# class sample_stratified
loadlibrary("caret")

sample_stratified <- function(data, label) {
  obj <- data_sample(data)
  obj$label <- label
  class(obj) <- append("sample_stratified", class(obj))  
  return(obj)
}

train_test.sample_stratified <- function(obj, perc=0.8) {
  predictors_name  = setdiff(colnames(obj$data), obj$label)
  
  predictors = obj$data[,predictors_name] 
  predictand = obj$data[,obj$label] 
  
  idx = createDataPartition(predictand, p=perc, list=FALSE)  
  obj$train = obj$data[idx,]
  obj$test = obj$data[-idx,]
  obj$data <- NULL
  return (obj)
}

ts_sample <- function(obj) {
  obj <- data_sample(obj$data)
  class(obj) <- append("ts_sample", class(obj))  
  return(obj)
}

train_test.ts_sample <- function(obj, test_size=1, offset=0) {
  offset <- nrow(obj$data) - test_size - offset
  obj$train <- obj$data[1:offset, ]
  obj$test <- obj$data[(offset+1):(offset+test_size),]
  if (ncol(obj$data) == 1) {
    obj$train <- as.matrix(obj$train)
    obj$test <- as.matrix(obj$test)
  }
  return (obj)
}

