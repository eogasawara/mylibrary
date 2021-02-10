# version 1.0
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myRelation.R")


# normalize normalization
normalize <- function(data) {
  obj <- rel_transform(data)
  class(obj) <- append("normalize", class(obj))    
  return(obj)
}  

deaction <- function(obj) {
  UseMethod("deaction")
}

deaction.default <- function(obj) {
  return(obj)
}

# min-max normalization
minmax <- function(data) {
  obj <- normalize(data)
  class(obj) <- append("minmax", class(obj))    
  return(obj)
}  

prepare.minmax <- function(obj) {
  data <- obj$data
  minmax = data.frame(t(ifelse(sapply(data, is.numeric), 1, 0)))
  minmax = rbind(minmax, rep(NA, ncol(minmax)))
  minmax = rbind(minmax, rep(NA, ncol(minmax)))
  colnames(minmax) = colnames(data)    
  rownames(minmax) = c("numeric", "max", "min")
  for (j in colnames(minmax)[minmax["numeric",]==1]) {
    minmax["min",j] <- min(data[,j], na.rm=TRUE)
    minmax["max",j] <- max(data[,j], na.rm=TRUE)
  }
  obj$norm.set <- minmax
  
  return(obj)
}

action.minmax <- function(obj) {
  data <- obj$data
  minmax <- obj$norm.set
  for (j in colnames(minmax)[minmax["numeric",]==1]) {
    if ((minmax["max", j] != minmax["min", j])) {
      data[,j] = (data[,j] - minmax["min", j]) / (minmax["max", j] - minmax["min", j])
    }
    else {
      data[,j] = 0
    }
  }
  return (data)
}

deaction.minmax <- function(obj) {
  data <- obj$data
  minmax <- obj$norm.set
  for (j in colnames(minmax)[minmax["numeric",]==1]) {
    if ((minmax["max", j] != minmax["min", j])) {
      data[,j] = data[,j] * (minmax["max", j] - minmax["min", j]) + minmax["min", j]
    }
    else {
      data[,j] = minmax["max", j]
    }
  }
  return (data)
}


# z-score normalization
zscore <- function(data, nmean=0, nsd=1) {
  obj <- normalize(data)
  obj$nmean <- nmean
  obj$nsd <- nsd
  class(obj) <- append("zscore", class(obj))    
  return(obj)
}  

prepare.zscore <- function(obj) {
  data <- obj$data
  nmean <- obj$nmean
  nsd <- obj$nsd
  zscore = data.frame(t(ifelse(sapply(data, is.numeric), 1, 0)))
  zscore = rbind(zscore, rep(NA, ncol(zscore)))
  zscore = rbind(zscore, rep(NA, ncol(zscore)))
  zscore = rbind(zscore, rep(NA, ncol(zscore)))
  zscore = rbind(zscore, rep(NA, ncol(zscore)))
  colnames(zscore) = colnames(data)    
  rownames(zscore) = c("numeric", "mean", "sd","nmean", "nsd")
  for (j in colnames(zscore)[zscore["numeric",]==1]) {
    zscore["mean",j] <- mean(data[,j], na.rm=TRUE)
    zscore["sd",j] <- sd(data[,j], na.rm=TRUE)
    zscore["nmean",j] <- nmean
    zscore["nsd",j] <- nsd
  }
  obj$norm.set <- zscore
  
  return(obj)  
}

action.zscore <- function(obj) {
  data <- obj$data
  zscore <- obj$norm.set
  for (j in colnames(zscore)[zscore["numeric",]==1]) {
    if ((zscore["sd", j]) > 0) {
      data[,j] = (data[,j] - zscore["mean", j]) / zscore["sd", j] * zscore["nsd", j] + zscore["nmean", j]
    }
    else {
      data[,j] = obj$nmean
    }
  }
  return (data)
}


deaction.zscore <- function(obj) {
  data <- obj$data
  zscore <- obj$norm.set
  for (j in colnames(zscore)[zscore["numeric",]==1]) {
    if ((zscore["sd", j]) > 0) {
      data[,j] = (data[,j] - zscore["nmean", j]) / zscore["nsd", j] * zscore["sd", j] + zscore["mean", j]
    }
    else {
      data[,j] = zscore["nmean", j]  
    }
  }
  return (data)
}

