data_outliers <- function(alpha = 1.5) {
  obj <- list(alpha = alpha)
  attr(obj, "class") <- "data_outliers"  
  return(obj)
}

outliers <- function(obj, data) {
  UseMethod("outliers")
}

outliers.default <- function(obj, data) {
  if(is.matrix(data) || is.data.frame(data)) {
    idx = rep(FALSE, nrow(data))
    org <- nrow(data)
    data <- as.data.frame(data)
    if (org >= 30) {
      isnumeric = (ifelse(sapply(data, is.numeric), TRUE, FALSE))
      data <- data[,as.vector(isnumeric)]
      q <- sapply(data, quantile, na.rm=TRUE)
      n <- ncol(data)
      for (i in 1:n)
      {
        IQR <- q[4,i] - q[2,i]
        lq1 <- q[2,i] - obj$alpha*IQR
        hq3 <- q[4,i] + obj$alpha*IQR
        idx = idx | (!is.na(data[,i]) & (data[,i] < lq1 | data[,i] > hq3))
      }
    }
    return(idx)
  }
  else {
    idx <- rep(FALSE, length(data))
    if (length(data) >= 30) {
      q <- quantile(data)
      IQR <- q[4] - q[2]
      lq1 <- q[2] - obj$alpha*IQR
      hq3 <- q[4] + obj$alpha*IQR
      idx <- data < lq1 | data > hq3
    }
    return (idx) 
  } 
}

outliers_remove <- function(obj, data) {
  UseMethod("outliers_remove")
}

outliers_remove.default <- function(obj, data)
{
  idx <- outliers(obj, data)
  if(is.matrix(data) || is.data.frame(data)) {
    return(data[!idx,])
  }
  else {
    return(data[!idx])
  } 
}
