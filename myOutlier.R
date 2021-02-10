# version 1.0
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myRelation.R")

outliers <- function(data, alpha = 1.5) {
  obj <- rel_transform(data)
  obj$alpha <- alpha
  class(obj) <- append("outliers", class(obj))    
  return(obj)
}

prepare.outliers <- function(obj) {
  data <- obj$data
  if(is.matrix(data) || is.data.frame(data)) {
    idx = rep(FALSE, nrow(data))
    org <- nrow(data)
    if (org >= 30) {
      for (i in 1:ncol(data)) {
        num <- is.numeric(data[,i])
        if (num) {
          q <- quantile(data[,i])
          IQR <- q[4] - q[2]
          lq1 <- q[2] - obj$alpha*IQR
          hq3 <- q[4] + obj$alpha*IQR
          idx = idx | (!is.na(data[,i]) & (data[,i] < lq1 | data[,i] > hq3))
        }
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
  if(is.matrix(obj$data))
    return(as.matrix(obj$data[!obj$idx,]))
  else if (is.data.frame(obj$data))
    return(as.data.frame(obj$data[!obj$idx,]))
  else 
    return(obj$data[!obj$idx])
}
