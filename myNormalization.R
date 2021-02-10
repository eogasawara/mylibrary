# version 1.0 
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myRelation.R")
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myOutlier.R")

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

# ts_normalize (base class)
ts_normalize <- function(data, scale = FALSE) {
  obj <- normalize(data)
  obj$sw <- ncol(data)
  obj$scale <- scale
  obj$scale_factor <- 1
  obj$scale_offset <- 0

  class(obj) <- append("ts_normalize", class(obj))    
  return(obj)
}

# ts_gminmax
ts_gminmax <- function(data, scale = FALSE) {
  obj <- ts_normalize(data, scale)
  class(obj) <- append("ts_gminmax", class(obj))    
  return(obj)
}

prepare.ts_gminmax <- function(obj) {
  out <- outliers(obj$data)
  out <- prepare(out)
  obj$data <- action(out)
  
  io <- ts_projection(obj)

  obj$gmin <- min(obj$data)
  obj$gmax <- max(obj$data)
  
  if (obj$scale) {
    swi_min <- apply(io$input, 1, min)
    swi_max <- apply(io$input, 1, max)
    
    swio_min <- apply(obj$data, 1, min)
    swio_max <- apply(obj$data, 1, max)
    
    ratio <- (swi_max-swi_min)/(swio_max-swio_min)
    out <- outliers(ratio)
    out <- prepare(out)
    ratio <- action(out)
    ratio <- mean(ratio)
    
    w <- (obj$gmax - obj$gmin)/(2*ratio)
    c <- (obj$gmax + obj$gmin)/2
    obj$gmax <- c + w
    obj$gmin <- c - w
  }
  
  return(obj)
}

action.ts_gminmax <- function(obj) {
  data <- obj$scale_factor*(obj$data-obj$gmin)/(obj$gmax-obj$gmin) + obj$scale_offset
  return(data)
}

deaction.ts_gminmax <- function(obj) {
  data <- (obj$data - obj$scale_offset) * (obj$gmax-obj$gmin) + obj$gmin
  return (data)
}



