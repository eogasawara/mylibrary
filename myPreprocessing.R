source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myGraphics.R")

loadlibrary("caret")
loadlibrary("MASS")
loadlibrary("DMwR")
loadlibrary("dplyr")

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
      q = quantile(data[,i], na.rm = TRUE)
      IQR = q[4] - q[2]
      lq1 = q[2] - alpha*IQR
      hq3 = q[4] + alpha*IQR
      out = out | (!is.na(data[,i]) & (data[,i] < lq1 | data[,i] > hq3))
    }
  }
  return (out)
}


# curvature analysis

curvature.min <- function(x, y, df=3, do_plot=TRUE) {
  smodel = smooth.spline(x, y, df = df)
  curvature = predict(smodel, x = x, deriv = 2)
  yv = min(curvature$y)
  xv = match(yv,curvature$y)
  if (do_plot) {
    variable = rep("values", length(x))
    variable[x==xv] <- "selected"
    res = data.frame(x=x, variable=variable, value=y)
    grfs <- plot.scatter(res,colors=c("red", "black"))   
    plot(grfs)
  }
  res = data.frame(x[xv], y[xv], yv)
  colnames(res) = c("x", "y", "z")
  return (res)
}


curvature.max <- function(x, y, df=3, do_plot=TRUE) {
  smodel = smooth.spline(x, y, df = df)
  curvature = predict(smodel, x = x, deriv = 2)
  yv = max(curvature$y)
  xv = match(yv,curvature$y)
  if (do_plot) {
    variable = rep("values", length(x))
    variable[x==xv] <- "selected"
    res = data.frame(x=x, variable=variable, value=y)
    grfs <- plot.scatter(res,colors=c("red", "black"))   
    plot(grfs)
  }
  res = data.frame(x[xv], y[xv], yv)
  colnames(res) = c("x", "y", "z")
  return (res)
}

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

# Data Transformation

# PCA
dt.pca <- function(data, class, transf = NULL, do_plot=FALSE)
{
  data = data.frame(data)
  if (class %in% colnames(data)) {
    predictand <- data[,class]
    data[,class] <- NULL
  } else {
    predictand <- NULL
  }
  
  if (!is.null(transf)) {
    pca.transf <- transf$pca.transf
    nums <- transf$nums
  } else {
    nums = unlist(lapply(data, is.numeric))
    remove <- NULL
    for(j in names(nums[nums])) {
      if(min(data[,j])==max(data[,j]))
        remove <- cbind(remove, j)
    }
    nums[remove] <- FALSE
  }
  
  data = as.matrix(data[ , nums])

  if (is.null(transf)) {
    pca_res = prcomp(data, center=TRUE, scale.=TRUE)
    cumvar = cumsum(pca_res$sdev^2/sum(pca_res$sdev^2))
    res = curvature.min(c(1:(length(cumvar))), cumvar, do_plot=do_plot)
    pca.transf = as.matrix(pca_res$rotation[, 1:res$x])
  }

  data = data %*% pca.transf
  data = data.frame(data)
  if (!is.null(predictand)){
    data[,class] <- predictand
  }
  transf=list(pca.transf=pca.transf, nums=nums)
  return (list(pca=data, transf=transf))
}

dt.categ_mapping <- function(data, attribute){
  mdlattribute = formula(paste("~", paste(attribute, "-1")))
  x <- model.matrix(mdlattribute, data=data)
  data <- cbind(data, x)
  return(data)
}

# Binning

binning <- function(v, interval) {
  names(interval) <- NULL
  interval[1] <- min(v)
  interval[length(interval)] <- max(v)
  interval.adj <- interval
  interval.adj[1] <- -.Machine$double.xmax
  interval.adj[length(interval)] <- .Machine$double.xmax
  
  vp <- cut(v, unique(interval.adj), FALSE, include.lowest=TRUE)
  m <- tapply(v, vp, mean)
  vm <- m[vp]
  mse <- mean((v - vm)^2, na.rm = TRUE)
  
  return (list(binning=m, bins_factor=vp, bins=vm, mse=mse, interval=interval, interval.adj=interval.adj))
}

# binning by interval

binning.interval <- function(v, n = NULL, interval=NULL, range=1.5) {
  if (is.null(interval)) {
    bp <- boxplot(v, range=range, plot = FALSE)
    bimax <- bp$stats[5]
    bimin <- bp$stats[1]
    if (bimin == bimax) {
      bimax = max(v)
      bimin = min(v)
    }
    interval <- seq(from = bimin, to = bimax, by = (bimax-bimin)/n)
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
      s <- stats::filter(s,rep(1/2,2), sides=2)[1:(n-1)]
      interval <- c(min(v), s, max(v))
    }
    else {
      interval <- c(min(v), max(v))
    }
  }
  return(binning(v,interval))
}

# optimizing binning

binning.opt <- function(v, binning=NULL, n=20, do_plot=FALSE) {
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
  res <- curvature.max(z$num, z$mean, do_plot = do_plot)
  return(interval[[res$x]])
}

entropy_group <- function(cluster, class) {
  tbl <- data.frame(x = cluster, y = class) %>% group_by(x, y) %>% summarise(qtd=n()) 
  tbs <- data.frame(x = cluster, y = class) %>% group_by(x) %>% summarise(t=n()) 
  tbl <- merge(x=tbl, y=tbs, by.x="x", by.y="x")
  tbl$e <- -(tbl$qtd/tbl$t)*log(tbl$qtd/tbl$t,2)
  tbl <- tbl %>% group_by(x) %>% summarise(ce=sum(e), qtd=sum(qtd)) 
  tbl$ceg <- tbl$ce*tbl$qtd/length(cluster)
  tbl <- sum(tbl$ceg)
  return (tbl)
}

# DATA BALANCING

balance.oversampling <- function(dataset, class) {
  x <- sort((table(dataset[,class]))) 
  class_formula = formula(paste(class, "  ~ ."))
  dataset[,class] <- as.character(dataset[,class])
  mainclass = names(x)[length(x)]
  newdata = NULL
  for (i in 1:(length(x)-1)) {
    minorclass = names(x)[i]
    curdata = dataset[dataset[,class]==mainclass | dataset[,class]==minorclass,]
    ratio <- as.integer(ceiling(x[length(x)]/x[i])*100)
    curdata[,class] <- as.factor(curdata[,class])
    curdata <- SMOTE(class_formula, curdata, perc.over = ratio, perc.under=100)
    curdata[,class] <- as.character(curdata[,class])
    curdata = curdata[curdata[,class]==minorclass, ]
    idx = sample(1:nrow(curdata),x[length(x)])
    curdata = curdata[idx,]
    newdata = rbind(newdata, curdata)
  }
  curdata = dataset[dataset[,class]==mainclass,]
  newdata = rbind(newdata, curdata)
  newdata[,class] <- as.factor(newdata[,class])
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

