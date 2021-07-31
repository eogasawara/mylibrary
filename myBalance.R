# version 1.2
#source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myData.R")

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
  library(smotefamily)
  j <- match(obj$attribute, colnames(data))
  x <- sort((table(data[,obj$attribute]))) 
  result <- data[data[obj$attribute]==names(x)[length(x)],]
  
  for (i in 1:(length(x)-1)) {
    small <- data[,obj$attribute]==names(x)[i]
    large <- data[,obj$attribute]==names(x)[length(x)]
    data_smote <- data[small | large,]
    syn_data <- SMOTE(data_smote[,-j], as.integer(data_smote[,j]))$syn_data
    syn_data$class <- NULL
    syn_data[obj$attribute] <- data[small, j][1]
    result <- rbind(result, data[small,])
    result <- rbind(result, syn_data)
  }
  return(result)
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
