loadlibrary <- function(x, repos='http://cran.fiocruz.br') 
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x, repos=repos, dep=TRUE)
    require(x)
  }
}

loadlibrary("caret")


# sampling

# classe sample

data_sample <- function(data) {
  obj <- list(data=data)
  attr(obj, "class") <- "data_sample"  
  return(obj)
}

train_test <- function(obj, perc=0.8) {
  UseMethod("train_test")
}

train_test.default <- function(obj, x) {
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
  return (obj)
}

# classe sample_random

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
  return (obj)
}

data(iris)

sr <- sample_random(iris)
sr <- train_test(sr)
sr <- k_fold(sr,4)

ss <- sample_stratified(iris, "Species")
ss <- train_test(ss)
ss <- k_fold(ss,4)


#samples <- sample.stratified(iris, "Species")
