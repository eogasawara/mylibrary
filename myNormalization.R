# version 1.0 
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myData.R")
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myOutlier.R")

# normalize normalization
normalize <- function() {
  obj <- dal_transform()
  class(obj) <- append("normalize", class(obj))    
  return(obj)
}  

deaction <- function(obj, ...) {
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

prepare.minmax <- function(obj, data) {
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

action.minmax <- function(obj, data) {
  minmax <- obj$norm.set
  for (j in colnames(minmax)[minmax["numeric",]==1]) {
    if ((minmax["max", j] != minmax["min", j])) {
      data[,j] <- (data[,j] - minmax["min", j]) / (minmax["max", j] - minmax["min", j])
    }
    else {
      data[,j] <- 0
    }
  }
  return (data)
}

deaction.minmax <- function(obj, data) {
  minmax <- obj$norm.set
  for (j in colnames(minmax)[minmax["numeric",]==1]) {
    if ((minmax["max", j] != minmax["min", j])) {
      data[,j] <- data[,j] * (minmax["max", j] - minmax["min", j]) + minmax["min", j]
    }
    else {
      data[,j] <- minmax["max", j]
    }
  }
  return (data)
}

# z-score normalization
zscore <- function(nmean=0, nsd=1) {
  obj <- normalize()
  obj$nmean <- nmean
  obj$nsd <- nsd
  class(obj) <- append("zscore", class(obj))    
  return(obj)
}  

prepare.zscore <- function(obj, data) {
  nmean <- obj$nmean
  nsd <- obj$nsd
  zscore <- data.frame(t(ifelse(sapply(data, is.numeric), 1, 0)))
  zscore <- rbind(zscore, rep(NA, ncol(zscore)))
  zscore <- rbind(zscore, rep(NA, ncol(zscore)))
  zscore <- rbind(zscore, rep(NA, ncol(zscore)))
  zscore <- rbind(zscore, rep(NA, ncol(zscore)))
  colnames(zscore) <- colnames(data)    
  rownames(zscore) <- c("numeric", "mean", "sd","nmean", "nsd")
  for (j in colnames(zscore)[zscore["numeric",]==1]) {
    zscore["mean",j] <- mean(data[,j], na.rm=TRUE)
    zscore["sd",j] <- sd(data[,j], na.rm=TRUE)
    zscore["nmean",j] <- nmean
    zscore["nsd",j] <- nsd
  }
  obj$norm.set <- zscore

  return(obj)  
}

action.zscore <- function(obj, data) {
  zscore <- obj$norm.set
  for (j in colnames(zscore)[zscore["numeric",]==1]) {
    if ((zscore["sd", j]) > 0) {
      data[,j] <- (data[,j] - zscore["mean", j]) / zscore["sd", j] * zscore["nsd", j] + zscore["nmean", j]
    }
    else {
      data[,j] <- obj$nmean
    }
  }
  return (data)
}

deaction.zscore <- function(obj, data) {
  zscore <- obj$norm.set
  for (j in colnames(zscore)[zscore["numeric",]==1]) {
    if ((zscore["sd", j]) > 0) {
      data[,j] <- (data[,j] - zscore["nmean", j]) / zscore["nsd", j] * zscore["sd", j] + zscore["mean", j]
    }
    else {
      data[,j] <- zscore["nmean", j]  
    }
  }
  return (data)
}

# ts_normalize (base class)
ts_normalize <- function(scale = FALSE) {
  obj <- normalize()
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

prepare.ts_gminmax <- function(obj, data) {
  out <- outliers()
  out <- prepare(out, data)
  data <- action(out, data)
  
  obj$gmin <- min(data)
  obj$gmax <- max(data)
  
  if (obj$scale && (ncol(data) > 1)) {
    input <- data[,1:(ncol(data)-1)]
    
    swi_min <- apply(input, 1, min)
    swi_max <- apply(input, 1, max)
    
    swio_min <- apply(data, 1, min)
    swio_max <- apply(data, 1, max)
    
    ratio <- (swi_max-swi_min)/(swio_max-swio_min)
    out <- outliers()
    out <- prepare(out, ratio)
    ratio <- action(out, ratio)
    ratio <- mean(ratio)
    
    w <- (obj$gmax - obj$gmin)/(2*ratio)
    c <- (obj$gmax + obj$gmin)/2
    obj$gmax <- c + w
    obj$gmin <- c - w
  }
  
  return(obj)
}

action.ts_gminmax <- function(obj, data, x=NULL) {
  if (!is.null(x)) {
    x <- (x-obj$gmin)/(obj$gmax-obj$gmin)
    return(x)
  }
  else {
    data <- (data-obj$gmin)/(obj$gmax-obj$gmin)
    return(data)
  }
}

deaction.ts_gminmax <- function(obj, data, x=NULL) {
  if (!is.null(x)) {
    x <- x * (obj$gmax-obj$gmin) + obj$gmin
    return(x)
  }
  else {
    data <- data * (obj$gmax-obj$gmin) + obj$gmin
    return (data)
  }
}

#ts_gminmax_diff
ts_gminmax_diff <- function(scale = FALSE) {
  obj <- ts_normalize(scale)
  class(obj) <- append("ts_gminmax_diff", class(obj))    
  return(obj)
}

prepare.ts_gminmax_diff <- function(obj, data) {
  data <- data[2:nrow(data),]-data[1:(nrow(data)-1),]
  obj <- prepare.ts_gminmax(obj, data)
  return(obj)
}

action.ts_gminmax_diff <- function(obj, data, x=NULL) {
  if (!is.null(x)) {
    ref <- attr(data, "ref")
    sw <- attr(data, "sw")
    if (sw == 1) {
      x <- x[2:nrow(x),]-ref
      x <- (x-obj$gmin)/(obj$gmax-obj$gmin)
    }
    else {
      x <- x-ref[,ncol(ref)]
      x <- (x-obj$gmin)/(obj$gmax-obj$gmin)
    }
    return(x)
  }
  else {
    ref <- NULL
    sw <- ncol(data) 
    if (sw == 1) {
      ref <- data[1:(nrow(data)-1),]
      data <- data[2:nrow(data),] - ref
      data <- (data-obj$gmin)/(obj$gmax-obj$gmin)
    }
    else {
      ref <- data[,1:(ncol(data)-1)]
      data <- data[,2:ncol(data)]-ref
      data <- (data-obj$gmin)/(obj$gmax-obj$gmin)
    }
    attr(data, "ref") <- ref
    attr(data, "sw") <- sw
    return(data)
  }
}

deaction.ts_gminmax_diff <- function(obj, data, x=NULL) {
  ref <- attr(data, "ref")
  sw <- attr(data, "sw")
  if (!is.null(x)) {
    x <- x * (obj$gmax-obj$gmin) + obj$gmin
    if (sw == 1) {
      x <- x[2:nrow(x),]+ref
    }
    else {
      x <- x+ref[,ncol(ref)]
    }
    return(x)
  }
  else {
    data <- data * (obj$gmax-obj$gmin) + obj$gmin
    if (sw == 1) {
      data <- rbind(ref[1, ], data + ref)
    }
    else {
      data <- cbind(ref[,1], (data + ref))
    }
    attr(data, "ref") <- ref
    attr(data, "sw") <- sw
    return(data)
  }
}

#ts_swminmax
ts_swminmax <- function(scale = FALSE) {
  obj <- ts_normalize(scale)
  class(obj) <- append("ts_swminmax", class(obj))    
  return(obj)
}

prepare.ts_swminmax <- function(obj, data) {
  out <- outliers()
  out <- prepare(out, data)
  data <- action(out, data)
  
  obj$scale_offset <- 0
  obj$scale_factor <- 1
  
  if (obj$scale) {
    input <- data[,1:(ncol(data)-1)]
    
    swi_min <- apply(input, 1, min)
    swi_max <- apply(input, 1, max)
    
    swio_min <- apply(data, 1, min)
    swio_max <- apply(data, 1, max)
    
    ratio <- (swi_max-swi_min)/(swio_max-swio_min)
    out <- outliers()
    out <- prepare(out, ratio)
    ratio <- action(out, ratio)
    q <- quantile(ratio)
    IQR <- q[4] - q[2]
    ratio <- q[2] - 1.5*IQR

    obj$scale_factor <- ratio
    obj$scale_offset <- (1 - ratio) / 2
  }
  return(obj)
}

action.ts_swminmax <- function(obj, data, x=NULL) {
  if (!is.null(x)) {
    i_min <- attr(data, "i_min")
    i_max <- attr(data, "i_max")
    x <- obj$scale_factor*(x-i_min)/(i_max-i_min) + obj$scale_offset
    return(x)
  }
  else {
    i_min <- apply(data, 1, min)
    i_max <- apply(data, 1, max)
    data <- obj$scale_factor*(data-i_min)/(i_max-i_min) + obj$scale_offset
    attr(data, "i_min") <- i_min
    attr(data, "i_max") <- i_max
    return(data)
  }
}

deaction.ts_swminmax <- function(obj, data, x=NULL) {
  i_min <- attr(data, "i_min")
  i_max <- attr(data, "i_max")
  if (!is.null(x)) {
    x <- (x - obj$scale_offset) * (i_max - i_min) / obj$scale_factor + i_min
    return(x)
  }
  else {
    data <- (data - obj$scale_offset) * (i_max - i_min) / obj$scale_factor + i_min
    attr(data, "i_min") <- i_min
    attr(data, "i_max") <- i_max
    return(data)
  }
}

#ts_an
ts_an <- function(scale = FALSE) {
  obj <- ts_normalize(scale)
  class(obj) <- append("ts_an", class(obj))    
  return(obj)
}

prepare.ts_an <- function(obj, data) {
  input <- data[,1:(ncol(data)-1)]
  an <- apply(input, 1, mean)
  data <- data / an

  out <- outliers()
  out <- prepare(out, data)
  data <- action(out, data)
  
  obj$gmin <- min(data)
  obj$gmax <- max(data)
  
  if (obj$scale && (ncol(data) > 1)) {
    input <- data[,1:(ncol(data)-1)]
    
    swi_min <- apply(input, 1, min)
    swi_max <- apply(input, 1, max)
    
    swio_min <- apply(data, 1, min)
    swio_max <- apply(data, 1, max)
    
    ratio <- (swi_max-swi_min)/(swio_max-swio_min)
    out <- outliers()
    out <- prepare(out, ratio)
    ratio <- action(out, ratio)
    ratio <- mean(ratio)
    
    w <- (obj$gmax - obj$gmin)/(2*ratio)
    c <- (obj$gmax + obj$gmin)/2
    obj$gmax <- c + w
    obj$gmin <- c - w
  }
  
  return(obj)
}

action.ts_an <- function(obj, data, x=NULL) {
  if (!is.null(x)) {
    an <- attr(data, "an")
    x <- x / an
    x <- (x - obj$gmin) / (obj$gmax-obj$gmin)
    return(x)
  }
  else {
    an <- apply(data, 1, mean)  
    data <- data / an
    data <- (data - obj$gmin) / (obj$gmax-obj$gmin) 
    attr(data, "an") <- an
    return (data)
  }
}

deaction.ts_an <- function(obj, data, x=NULL) {
  an <- attr(data, "an")
  if (!is.null(x)) {
    x <- x * (obj$gmax-obj$gmin) + obj$gmin
    x <- x * an
    return(x)
  }
  else {
    data <- data * (obj$gmax-obj$gmin) + obj$gmin
    data <- data * an
    attr(data, "an") <- an
    return (data)
  }
}

exp_mean <- function(x) {
  n <- length(x)
  y <- rep(0,n)
  alfa <- 1 - 2.0 / (n + 1);
  for (i in 0:(n-1)) {
    y[n-i] <- alfa^i
  }
  m <- sum(y * x)/sum(y)
  return(m)
}




