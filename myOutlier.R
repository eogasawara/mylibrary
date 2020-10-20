source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myRel.R")

outliers <- function(data, alpha = 1.5, prepare=TRUE) {
  obj <- rel_transform(data)
  obj$alpha <- alpha
  class(obj) <- append("outliers", class(obj))    
  if (prepare)
    obj <- prepare(obj)
  return(obj)
}

prepare.outliers <- function(obj) {
  data <- obj$data
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
    obj$idx <- idx
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
    obj$idx <- idx
  } 
  return(obj)
}

action.outliers <- function(obj)
{
  if(is.matrix(obj$data) || is.data.frame(obj$data)) {
    return(obj$data[!obj$idx,])
  }
  else {
    return(obj$data[!obj$idx])
  } 
}
