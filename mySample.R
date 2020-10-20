loadlibrary <- function(x, repos='http://cran.fiocruz.br') 
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x, repos=repos, dep=TRUE)
    require(x)
  }
}

# class sample

data_sample <- function(data) {
  obj <- rel_transform(data)
  class(obj) <- append("data_sample", class(obj))    
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
  return (obj)
}

#class oversampling
loadlibrary("DMwR")

oversampling <- function(data, class) {
  obj <- data_sample(as.data.frame(data))
  obj$class <- class
  class(obj) <- append("oversampling", class(obj))    
  return(obj)
}

action.oversampling <- function(obj) {
  data <- obj$data
  class <- obj$class
  
  x <- sort((table(data[,class]))) 
  class_formula = formula(paste(class, "  ~ ."))
  data[,class] <- as.character(data[,class])
  mainclass = names(x)[length(x)]
  newdata = NULL
  for (i in 1:(length(x)-1)) {
    minorclass = names(x)[i]
    curdata = data[data[,class]==mainclass | data[,class]==minorclass,]
    ratio <- as.integer(ceiling(x[length(x)]/x[i])*100)
    curdata[,class] <- as.factor(curdata[,class])
    curdata <- SMOTE(class_formula, curdata, perc.over = ratio, perc.under=100)
    curdata[,class] <- as.character(curdata[,class])
    curdata = curdata[curdata[,class]==minorclass, ]
    idx = sample(1:nrow(curdata),x[length(x)])
    curdata = curdata[idx,]
    newdata = rbind(newdata, curdata)
  }
  curdata = data[data[,class]==mainclass,]
  newdata = rbind(newdata, curdata)
  newdata[,class] <- as.factor(newdata[,class])
  return(newdata)
}

subsampling <- function(data, class) {
  obj <- data_sample(as.data.frame(data))
  obj$class <- class
  class(obj) <- append("subsampling", class(obj))    
  return(obj)
}

action.subsampling <- function(obj) {
  data <- obj$data
  class <- obj$class
  
  x <- sort((table(data[,class]))) 
  qminor = as.integer(x[1])
  newdata = NULL
  for (i in 1:length(x)) {
    cclass = names(x)[i]
    curdata = data[data[,class]==cclass,]
    idx = sample(1:nrow(curdata),qminor)
    curdata = curdata[idx,]
    newdata = rbind(newdata, curdata)
  }
  return(newdata)
}

