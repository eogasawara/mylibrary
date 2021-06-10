# version 1.0
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myData.R")

# data_sample

data_sample <- function() {
  obj <- list()
  attr(obj, "class") <- "data_sample"  
  return(obj)
}

train_test <- function(obj, data, ...) {
  UseMethod("train_test")
}

train_test.default <- function(obj, data) {
  return(list())
}

k_fold <- function(obj, data, k) {
  UseMethod("k_fold")
}

k_fold.default <- function(obj, data, k) {
  return(list())
}


# sample_random

sample_random <- function() {
  obj <- data_sample()
  class(obj) <- append("sample_random", class(obj))  
  return(obj)
}

train_test.sample_random <- function(obj, data, perc=0.8) {
  idx <- base::sample(1:nrow(data),as.integer(perc*nrow(data)))
  train <- data[idx,]
  test <- data[-idx,]
  return (list(train=train, test=test))
}

k_fold.sample_random <- function(obj, data, k) {
  folds <- list()
  samp <- list()
  p <- 1.0 / k
  while (k > 1) {
    samp <- train_test(obj, data, p)
    data <- samp$test
    folds <- append(folds, list(samp$train))
    k = k - 1
    p = 1.0 / k
  }
  folds <- append(folds, list(samp$test))
  return (folds)
}

train_test_from_folds <- function(folds, k) {
  test <- folds[[k]]
  train <- NULL
  for (i in 1:length(folds)) {
    if (i != k)
      train <- rbind(train, folds[[i]])
  }
  return (list(train=train, test=test))
}

# sample_stratified
sample_stratified <- function(attribute) {
  obj <- sample_random()
  obj$attribute <- attribute
  class(obj) <- append("sample_stratified", class(obj))  
  return(obj)
}

train_test.sample_stratified <- function(obj, data, perc=0.8) {
  loadlibrary("caret")
  
  predictors_name <- setdiff(colnames(data), obj$attribute)
  predictand <- data[,obj$attribute] 
  
  idx <- createDataPartition(predictand, p=perc, list=FALSE) 
  train <- data[idx,]
  test <- data[-idx,]
  return (list(train=train, test=test))
}

