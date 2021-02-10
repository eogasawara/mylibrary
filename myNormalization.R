# version 1.0 
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myRelation.R")
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myOutlier.R")

# normalize normalization
normalize <- function() {
  obj <- rel_transform(NULL)
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
minmax <- function() {
  obj <- normalize()
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
  obj$data <- data
  return (obj)
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
  obj$data <- data
  return (obj)
}

# z-score normalization
zscore <- function(nmean=0, nsd=1) {
  obj <- normalize()
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
  obj$data <- data
  return (obj)
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
  obj$data <- data
  return (obj)
}

# ts_normalize (base class)
ts_normalize <- function(scale = FALSE) {
  obj <- normalize()
  obj$sw <- ncol(data)
  obj$scale <- scale

  class(obj) <- append("ts_normalize", class(obj))    
  return(obj)
}

# ts_gminmax
ts_gminmax <- function(scale = FALSE) {
  obj <- ts_normalize(scale)
  class(obj) <- append("ts_gminmax", class(obj))    
  return(obj)
}

prepare.ts_gminmax <- function(obj) {
  out <- outliers(obj$data)
  out <- prepare(out)
  obj$data <- action(out)
  
  obj$gmin <- min(obj$data)
  obj$gmax <- max(obj$data)
  
  if (obj$scale) {
    io <- ts_projection(obj)
    
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
  obj$data <- (obj$data-obj$gmin)/(obj$gmax-obj$gmin)
  return(obj)
}

deaction.ts_gminmax <- function(obj) {
  obj$data <- obj$data * (obj$gmax-obj$gmin) + obj$gmin
  return (obj)
}

#ts_gminmax_diff
ts_gminmax_diff <- function(scale = FALSE) {
  obj <- ts_normalize(scale)
  class(obj) <- append("ts_gminmax_diff", class(obj))    
  return(obj)
}

prepare.ts_gminmax_diff <- function(obj) {
  data <- adjust.matrix(obj$data[2:nrow(obj$data),]-obj$data[1:(nrow(obj$data)-1),])
  
  out <- outliers(data)
  out <- prepare(out)
  data <- action(out)
  obj$data <- adjust.matrix(obj$data[!c(FALSE, out$idx),])
  
  obj$gmin <- min(data)
  obj$gmax <- max(data)

  if (obj$scale) {
    io <- ts_projection(obj)
    
    swi_min <- apply(io$input, 1, min)
    swi_max <- apply(io$input, 1, max)
    
    swio_min <- apply(data, 1, min)
    swio_max <- apply(data, 1, max)
    
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

action.ts_gminmax_diff <- function(obj) {
  obj$ref <- adjust.matrix(obj$data[1:(nrow(obj$data)-1),])
  obj$data <- adjust.matrix(obj$data[2:nrow(obj$data),]-obj$ref)
  obj$data <- (obj$data-obj$gmin)/(obj$gmax-obj$gmin)
  return(obj)
}

deaction.ts_gminmax_diff <- function(obj) {
  obj$data <- obj$data * (obj$gmax-obj$gmin) + obj$gmin
  obj$data <- rbind(obj$ref[1, ], obj$data + obj$ref)
  return (obj)
}
