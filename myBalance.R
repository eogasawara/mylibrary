# version 1.0
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/mySample.R")


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

oversampling <- function(data, label) {
  obj <- data_sample(as.data.frame(data))
  obj$label <- label
  class(obj) <- append("oversampling", class(obj))    
  return(obj)
}

action.oversampling <- function(obj) {
  data <- obj$data
  label <- obj$label
  
  x <- sort((table(data[,label]))) 
  class_formula = formula(paste(label, "  ~ ."))
  data[,label] <- as.character(data[,label])
  mainclass = names(x)[length(x)]
  newdata = NULL
  for (i in 1:(length(x)-1)) {
    minorclass = names(x)[i]
    curdata = data[data[,label]==mainclass | data[,label]==minorclass,]
    ratio <- as.integer(ceiling(x[length(x)]/x[i])*100)
    curdata[,label] <- as.factor(curdata[,label])
    curdata <- SMOTE(class_formula, curdata, perc.over = ratio, perc.under=100)
    curdata[,label] <- as.character(curdata[,label])
    curdata = curdata[curdata[,label]==minorclass, ]
    idx = sample(1:nrow(curdata),x[length(x)])
    curdata = curdata[idx,]
    newdata = rbind(newdata, curdata)
  }
  curdata = data[data[,label]==mainclass,]
  newdata = rbind(newdata, curdata)
  newdata[,label] <- as.factor(newdata[,label])
  return(newdata)
}

subsampling <- function(data, label) {
  obj <- data_sample(as.data.frame(data))
  obj$label <- label
  class(obj) <- append("subsampling", class(obj))    
  return(obj)
}

action.subsampling <- function(obj) {
  data <- obj$data
  label <- obj$label
  x <- sort((table(data[,label]))) 
  qminor = as.integer(x[1])
  newdata = NULL
  for (i in 1:length(x)) {
    curdata = data[data[,label]==(names(x)[i]),]
    idx = sample(1:nrow(curdata),qminor)
    curdata = curdata[idx,]
    newdata = rbind(newdata, curdata)
  }
  return(newdata)
}

