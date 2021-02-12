# version 1.0
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myData.R")

# data_sample

data_sample <- function() {
  obj_sample <- list()
  attr(obj_sample, "class") <- "data_sample"  
  return(obj_sample)
}

train_test <- function(obj_sample, obj_data, ...) {
  UseMethod("train_test")
}

train_test.default <- function(obj_sample, obj_data) {
  return(list())
}

k_fold <- function(obj_sample, obj_data, k) {
  UseMethod("k_fold")
}

k_fold.default <- function(obj_sample, obj_data, k) {
  return(list())
}


# sample_random

sample_random <- function() {
  obj_sample <- data_sample()
  class(obj_sample) <- append("sample_random", class(obj_sample))  
  return(obj_sample)
}

train_test.sample_random <- function(obj_sample, obj_data, perc=0.8) {
  idx <- base::sample(1:nrow(obj_data$data),as.integer(perc*nrow(obj_data$data)))
  obj_train <- obj_data
  obj_train$data <- obj_data$data[idx,]
  obj_test <- obj_data
  obj_test$data <- obj_data$data[-idx,]
  return (list(train=obj_train, test=obj_test))
}

k_fold.sample_random <- function(obj_sample, obj_data, k) {
  folds <- list()
  p <- 1.0 / k
  while (k > 1) {
    obj <- train_test(obj_sample, obj_data, p)
    obj_data <- obj$test
    folds <- append(folds, list(obj$train))
    k = k - 1
    p = 1.0 / k
  }
  folds <- append(folds, list(obj$test))
  return (folds)
}

# sample_stratified
sample_stratified <- function(attribute) {
  obj_sample <- sample_random()
  obj_sample$attribute <- attribute
  class(obj_sample) <- append("sample_stratified", class(obj_sample))  
  return(obj_sample)
}

train_test.sample_stratified <- function(obj_sample, obj_data, perc=0.8) {
  loadlibrary("caret")
  
  predictors_name <- setdiff(colnames(obj_data$data), obj_sample$attribute)
  predictand <- obj_data$data[,obj_sample$attribute] 
  
  idx <- createDataPartition(predictand, p=perc, list=FALSE) 
  obj_train <- obj_data
  obj_train$data <- obj_data$data[idx,]
  obj_test <- obj_data
  obj_test$data <- obj_data$data[-idx,]
  return (list(train=obj_train, test=obj_test))
}

