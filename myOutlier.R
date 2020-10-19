outliers.boxplot <- function(x, alpha = 1.5, idx=FALSE)
{
  ismatrix <- is.matrix(x)
  if(ismatrix || is.data.frame(x)) {
    out = rep(FALSE, nrow(x))
    x <- data.frame(x)
    x <- na.omit(x)
    org <- nrow(x)
    if (org >= 30) {
      q <- as.data.frame(lapply(x, quantile, na.rm=TRUE))
      n <- ncol(x)
      for (i in 1:n)
      {
        IQR <- q[4,i] - q[2,i]
        lq1 <- q[2,i] - alpha*IQR
        hq3 <- q[4,i] + alpha*IQR
        out = out | (!is.na(x[,i]) & (x[,i] < lq1 | x[,i] > hq3))
      }
    }
    if (idx)
      x <- out
    else
      x <- x[!out,]
    if (ismatrix)
      x <- as.matrix(x)
  }
  else {
    if (length(x) >= 30) {
      q <- quantile(x)
      IQR <- q[4] - q[2]
      lq1 <- q[2] - alpha*IQR
      hq3 <- q[4] + alpha*IQR
      cond <- x >= lq1 & x <= hq3
    }
    else
      out <- rep(FALSE, length(x))
    if (idx)
      x <- out
    else
      x <- x[!out]
  } 
  return (x)
}


