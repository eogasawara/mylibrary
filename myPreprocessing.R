loadlibrary <- function(x) 
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x, repos='http://cran.fiocruz.br', dep=TRUE)
    loadlibrary(x)
  }
}

loadlibrary("caret")
loadlibrary("MASS")
loadlibrary("DMwR")

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
}

# Data Transformation

# PCA

dt.pca <- function(data, class, transf = NULL)
{
  data = data.frame(data)
  if (!is.numeric(data[,class]))
    data[,class] =  as.numeric(data[,class])
  nums = unlist(lapply(data, is.numeric))
  data = data[ , nums]
  
  predictors_name  = setdiff(colnames(data), class)
  predictors = as.matrix(data[,predictors_name])
  if (is.null(transf)) {
    predictand = data[,class]
    pca_res = prcomp(predictors, center=TRUE, scale.=TRUE)
    cumvar = cumsum(pca_res$sdev^2/sum(pca_res$sdev^2))
    res = curvature.min(c(1:(length(cumvar))), cumvar)
    transf = as.matrix(pca_res$rotation[, 1:res$x])
  }
  
  dataset = predictors %*% transf
  dataset = data.frame(dataset)
  return (list(pca=dataset, pca.transf=transf))
}

dt.categ_mapping <- function(data, attribute){
  mdlattribute = formula(paste("~", paste(attribute, "-1")))
  x <- model.matrix(mdlattribute, data=data)
  data <- cbind(data, x)
  data[,attribute] <- NULL
  return(data)
}

# Binning

binning <- function(v, interval) {
  interval[1] <- min(v)
  interval[length(interval)] <- max(v)
  interval.adj <- interval
  interval.adj[1] <- .Machine$double.xmin
  interval.adj[length(interval)] <- .Machine$double.xmax
  
  vp <- cut(v, unique(interval.adj), FALSE, include.lowest=TRUE)
  m <- tapply(v, vp, mean)
  vm <- m[vp]
  mse <- mean((v - vm)^2, na.rm = TRUE)
  
  return (list(binning=m, bins_factor=vp, bins=vm, mse=mse, interval=interval, interval.adj=interval.adj))
}

# binning by interval

binning.interval <- function(v, n = NULL, 
                             interval=NULL, range=1.5) {
  if (is.null(interval)) {
    bp <- boxplot(v, range=range, plot = FALSE)
    interval <- seq(from = bp$stats[1], to = bp$stats[5], 
        by = (bp$stats[5]-bp$stats[1])/n)
  }
  return(binning(v,interval))
}

# binning by freq

binning.freq <- function(v, n = NULL, interval=NULL) {
  if (is.null(interval)) {
    p <- seq(from = 0, to = 1, by = 1/n)
    interval <- quantile(v, p)
  }
  return(binning(v,interval))
}

# binning by cluster

binning.cluster <- function(v, n = NULL, interval=NULL) {
  if (is.null(interval)) {
    if (n > 1) {
      km <- kmeans(x = v, centers = n)
      s <- sort(km$centers)
      s <- filter(s,rep(1/2,2), sides=2)[1:(n-1)]
      interval <- c(min(v), s, max(v))
    }
    else {
      interval <- c(min(v), max(v))
    }
  }
  return(binning(v,interval))
}

# optimizing binning

binning.opt <- function(v, binning=NULL, n=20) {
  z <- data.frame()
  interval <- list()
  for (i in 1:n)
  {
    t <- binning(v, i)
    interval = append(interval, list(t))
    newrow <- c(t$mse , i)
    z <- rbind(z,newrow)
  }
  colnames(z)<-c("mean","num") 
  res <- curvature.max(z$num, z$mean)
  return(interval[[res$x]])
}


# DATA BALANCING

balance.oversampling <- function(data, class) {
  x <- sort((table(data[,class]))) 
  class_formula = formula(paste(class, "  ~ ."))
  mainclass = names(x)[length(x)]
  newdata = NULL
  for (i in 1:(length(x)-1)) {
    minorclass = names(x)[i]
    curdata = data[data[,class]==mainclass
                   | data[,class]==minorclass,]
    curdata$Species <- factor(curdata$Species) 
    ratio <- as.integer(ceiling(x[length(x)]/x[i])*100)
    curdata <- SMOTE(class_formula, curdata, 
                     perc.over = ratio, perc.under=100)
    curdata = curdata[curdata[,class]==minorclass, ]
    idx = sample(1:nrow(curdata),x[length(x)])
    curdata = curdata[idx,]
    newdata = rbind(newdata, curdata)
  }
  curdata = data[data[,class]==mainclass,]
  newdata = rbind(newdata, curdata)
  return(newdata)
}

balance.subsampling <- function(data, class) {
  x <- sort((table(data[,class]))) 
  qminor = as.integer(x[1])
  newdata = NULL
  for (i in 1:length(x)) {
    cclass = names(x)[i]
    curdata = data[data[,class]==cclass,]
    idx = sample(1:nrow(curdata),qminor)
    curdata = curdata[idx,]
    newdata = rbind(newdata, curdata)
  }
  return(newdata)
}

