# min-max normalization

normalize.minmax <- function(data, norm.set=NULL){
  data = data.frame(data)
  if(is.null(norm.set))
  {
    minmax = data.frame(t(ifelse(sapply(data, is.numeric), 1, 0)))
    minmax = rbind(minmax, rep(NA, ncol(minmax)))
    minmax = rbind(minmax, rep(NA, ncol(minmax)))
    colnames(minmax) = colnames(data)    
    rownames(minmax) = c("numeric", "max", "min")
    for (j in colnames(minmax)[minmax["numeric",]==1]) {
      minmax["min",j] <- min(data[,j], na.rm=TRUE)
      minmax["max",j] <- max(data[,j], na.rm=TRUE)
    }
  }
  else {
    minmax = norm.set
  }
  for (j in colnames(minmax)[minmax["numeric",]==1]) {
    if ((minmax["max", j] != minmax["min", j])) {
      data[,j] = (data[,j] - minmax["min", j]) / (minmax["max", j] - minmax["min", j])
    }
    else {
      data[,j] = 0
    }
  }
  return (list(data=data, norm.set=minmax))
}

# z-score normalization

normalize.zscore <- function(data, norm.set=NULL, nmean=0, nsd=1){
  data = data.frame(data)
  if(is.null(norm.set))
  {
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
  }
  else {
    zscore = norm.set
  }
  for (j in colnames(zscore)[zscore["numeric",]==1]) {
    if ((zscore["sd", j]) > 0) {
      data[,j] = ((data[,j] - zscore["mean", j]) / zscore["sd", j]) * zscore["nsd", j] + zscore["nmean", j]
    }
    else {
      data[,j] = zscore["nmean", j]  
    }
  }
  return (list(data=data, norm.set=zscore))
}