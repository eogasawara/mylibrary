loadlibrary <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x, repos='http://cran.fiocruz.br', dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

loadlibrary("caret")


# samples

sample.random <- function(data, perc=0.8)
{
  idx = sample(1:nrow(data),as.integer(perc*nrow(data)))
  sample = data[idx,]
  residual = data[-idx,]
  return (list(sample=sample, residual=residual))
}

sample.stratified <- function(data, clabel, perc=0.8)
{
  predictors_name  = setdiff(colnames(data), clabel)
  
  predictors = data[,predictors_name] 
  predictand = data[,clabel] 
  
  idx = createDataPartition(predictand, p=perc, list=FALSE)  
  sample = data[idx,]
  residual = data[-idx,]
  return (list(sample=sample, residual=residual))
}

sample.random_kfold <- function(data, k=10)
{
  sets = list()
  p = 1.0 / k
  while (k > 1) {
    samples = sample.random(data, p)
    fold = samples[[1]]
    data = samples[[2]]
    sets = append(sets, list(fold))
    k = k - 1
    p = 1.0 / k
  }
  sets = append(sets, list(data))
  return (sets)
}

sample.stratified_kfold <- function(data, clabel, k=10)
{
  sets = list()
  p = 1.0 / k
  while (k > 1) {
    samples = sample.stratified(data, clabel, p)
    fold = samples[[1]]
    data = samples[[2]]
    sets = append(sets, list(fold))
    k = k - 1
    p = 1.0 / k
  }
  sets = append(sets, list(data))
  return (sets)
}


# outlier analysis

outliers.boxplot <- function(data, alpha = 1.5)
{
  out = rep(FALSE, nrow(data))
  nums = unlist(lapply(data, is.numeric))
  for (i in 1:ncol(data))
  {
    if (nums[i])
    {
      q = quantile(data[,i])
      IQR = q[4] - q[2]
      lq1 = q[2] - alpha*IQR
      hq3 = q[4] + alpha*IQR
      out = out | data[,i] < lq1 | data[,i] > hq3
    }
  }
  return (out)
}


# curvature analysis

curvature.max <- function(x, y, df=3) {
  smodel = smooth.spline(x, y, df = df)
  curvature = predict(smodel, x = x, deriv = 2)
  yv = max(curvature$y)
  xv = match(yv,curvature$y)
  plot(x, y)
  points(x[xv], y[xv], pch=19)
  res = data.frame(x[xv], y[xv], yv)
  colnames(res) = c("x", "y", "z")
  return (res)
}

curvature.min <- function(x, y, df=3) {
  smodel = smooth.spline(x, y, df = df)
  curvature = predict(smodel, x = x, deriv = 2)
  yv = min(curvature$y)
  xv = match(yv,curvature$y)
  plot(x, y)
  points(x[xv], y[xv], pch=19)
  res = data.frame(x[xv], y[xv], yv)
  colnames(res) = c("x", "y", "z")
  return (res)
}


# min-max normalization

normalize.minmax <- function(data, norm.set=NULL)
{
  data = data.frame(data)
  nums = unlist(lapply(data, is.numeric))
  data = data[ , nums]
  
  if(is.null(norm.set))
  {
    minmax = data.frame(t(sapply(data, max, na.rm=TRUE)))
    minmax = rbind(minmax, t(sapply(data, min, na.rm=TRUE)))
    colnames(minmax) = colnames(data)    
    rownames(minmax) = c("max", "min")
  }
  else {
    minmax = norm.set
  }
  for (i in 1:ncol(data))
    data[,i] = (data[,i] - minmax["min", i]) / (minmax["max", i] - minmax["min", i])
  return (list(data=data, norm.set=minmax))
}


# z-score normalization

normalize.zscore <- function(data, norm.set=NULL, nmean=0, nsd=1)
{
  data = data.frame(data)
  nums = unlist(lapply(data, is.numeric))
  data = data[ , nums]

  if(is.null(norm.set))
  {
    zscore <- matrix(nrow = 4, ncol=ncol(data))
    zscore[1,] = t(sapply(data, mean, na.rm=TRUE))
    zscore[2,] = t(sapply(data, sd, na.rm=TRUE))
    zscore[3,] = t(rep(nmean, ncol(data)))
    zscore[4,] = t(rep(nsd, ncol(data)))
    zscore = data.frame(zscore)
    colnames(zscore) = colnames(data)    
    rownames(zscore) = c("mean", "sd","nmean", "nsd")
  }
  for (i in 1:ncol(data))
    data[,i] = ((data[,i] - zscore["mean", i]) / zscore["sd", i]) * zscore["nsd", i] + zscore["nmean", i]
  return (list(data=data, norm.set=zscore))
  return (list(data, zscore))
}

