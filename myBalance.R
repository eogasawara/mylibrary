# version 1.0
source("myData.R")

#balance

balance_dataset <- function(attribute) {
  obj <- list(attribute=attribute)
  attr(obj, "class") <- "balance_dataset"  
  return(obj)
}

balance <- function(obj, data) {
  UseMethod("balance")
}

balance.default <- function(obj, data) {
  return(list())
}

#balance_oversampling

balance_oversampling <- function(attribute) {
  obj <- balance_dataset(attribute)
  class(obj) <- append("balance_oversampling", class(obj))    
  return(obj)
}

balance.balance_oversampling <- function(obj, data) {
  loadlibrary("DMwR")
  
  data <- data
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
  data <- newdata
  return(data)
}

# balance_subsampling
balance_subsampling <- function(attribute) {
  obj <- balance_dataset(attribute)
  class(obj) <- append("balance_subsampling", class(obj))    
  return(obj)
}

balance.balance_subsampling <- function(obj, data) {
  data <- data
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
  data <- newdata
  return(data)
}
