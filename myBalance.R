# version 1.0
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/mySample.R")


# class sample_stratified
loadlibrary("caret")

sample_stratified <- function(data, attribute) {
  obj <- data_sample(data)
  obj$attribute <- attribute
  class(obj) <- append("sample_stratified", class(obj))  
  return(obj)
}

train_test.sample_stratified <- function(obj, perc=0.8) {
  predictors_name  = setdiff(colnames(obj$data), obj$attribute)
  
  predictors = obj$data[,predictors_name] 
  predictand = obj$data[,obj$attribute] 
  
  idx = createDataPartition(predictand, p=perc, list=FALSE)  
  obj$train = obj$data[idx,]
  obj$test = obj$data[-idx,]
  return (obj)
}

#class oversampling
loadlibrary("DMwR")

oversampling <- function(data, attribute) {
  obj <- data_sample(as.data.frame(data))
  obj$attribute <- attribute
  class(obj) <- append("oversampling", class(obj))    
  return(obj)
}

action.oversampling <- function(obj) {
  data <- obj$data
  attribute <- obj$attribute
  
  x <- sort((table(data[,attribute]))) 
  class_formula = formula(paste(attribute, "  ~ ."))
  data[,attribute] <- as.character(data[,attribute])
  mainclass = names(x)[length(x)]
  newdata = NULL
  for (i in 1:(length(x)-1)) {
    minorclass = names(x)[i]
    curdata = data[data[,attribute]==mainclass | data[,attribute]==minorclass,]
    ratio <- as.integer(ceiling(x[length(x)]/x[i])*100)
    curdata[,attribute] <- as.factor(curdata[,attribute])
    curdata <- SMOTE(class_formula, curdata, perc.over = ratio, perc.under=100)
    curdata[,attribute] <- as.character(curdata[,attribute])
    curdata = curdata[curdata[,attribute]==minorclass, ]
    idx = sample(1:nrow(curdata),x[length(x)])
    curdata = curdata[idx,]
    newdata = rbind(newdata, curdata)
  }
  curdata = data[data[,attribute]==mainclass,]
  newdata = rbind(newdata, curdata)
  newdata[,attribute] <- as.factor(newdata[,attribute])
  return(newdata)
}

subsampling <- function(data, attribute) {
  obj <- data_sample(as.data.frame(data))
  obj$attribute <- attribute
  class(obj) <- append("subsampling", class(obj))    
  return(obj)
}

action.subsampling <- function(obj) {
  data <- obj$data
  attribute <- obj$attribute
  x <- sort((table(data[,attribute]))) 
  qminor = as.integer(x[1])
  newdata = NULL
  for (i in 1:length(x)) {
    curdata = data[data[,attribute]==(names(x)[i]),]
    idx = sample(1:nrow(curdata),qminor)
    curdata = curdata[idx,]
    newdata = rbind(newdata, curdata)
  }
  return(newdata)
}

