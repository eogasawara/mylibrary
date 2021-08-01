# version 1.2
# depends myBasic.R

### Balance Dataset

balance_dataset <- function(attribute) {
  obj <- list(attribute=attribute)
  attr(obj, "class") <- "balance_dataset"  
  return(obj)
}

balance <- function(obj, data) {
  UseMethod("balance")
}

balance.default <- function(obj, data) {
  return(list())
}

#balance_oversampling

balance_oversampling <- function(attribute) {
  obj <- balance_dataset(attribute)
  class(obj) <- append("balance_oversampling", class(obj))    
  return(obj)
}

balance.balance_oversampling <- function(obj, data) {
  library(smotefamily)
  j <- match(obj$attribute, colnames(data))
  x <- sort((table(data[,obj$attribute]))) 
  result <- data[data[obj$attribute]==names(x)[length(x)],]
  
  for (i in 1:(length(x)-1)) {
    small <- data[,obj$attribute]==names(x)[i]
    large <- data[,obj$attribute]==names(x)[length(x)]
    data_smote <- data[small | large,]
    syn_data <- SMOTE(data_smote[,-j], as.integer(data_smote[,j]))$syn_data
    syn_data$class <- NULL
    syn_data[obj$attribute] <- data[small, j][1]
    result <- rbind(result, data[small,])
    result <- rbind(result, syn_data)
  }
  return(result)
}

# balance_subsampling
balance_subsampling <- function(attribute) {
  obj <- balance_dataset(attribute)
  class(obj) <- append("balance_subsampling", class(obj))    
  return(obj)
}

balance.balance_subsampling <- function(obj, data) {
  data <- data
  attribute <- obj$attribute
  x <- sort((table(data[,attribute]))) 
  qminor = as.integer(x[1])
  newdata = NULL
  for (i in 1:length(x)) {
    curdata = data[data[,attribute]==(names(x)[i]),]
    idx = sample(1:nrow(curdata),qminor)
    curdata = curdata[idx,]
    newdata = rbind(newdata, curdata)
  }
  data <- newdata
  return(data)
}


### Categorical Mapping


categ_mapping <- function(attribute) {
  obj <- dal_transform()
  obj$attribute <- attribute
  class(obj) <- append("categ_mapping", class(obj))  
  return(obj)  
}

action.categ_mapping <- function(obj, data) {
  mdlattribute = formula(paste("~", paste(obj$attribute, "-1")))
  catmap <- model.matrix(mdlattribute, data=data)
  data <- cbind(data, catmap)
  return(data)
}

### Fitting

fit_curvature <- function() {
  obj <- dal_transform()
  obj$df <- 2
  obj$deriv <- 2
  class(obj) <- append("fit_curvature", class(obj))    
  return(obj)
}

action.fit_curvature <- function(obj, y) {
  x <- 1:length(y)
  smodel = smooth.spline(x, y, df = obj$df)
  curvature = predict(smodel, x = x, deriv = obj$deriv)
  yfit = obj$func(curvature$y)
  xfit = match(yfit, curvature$y)
  y <- y[xfit]
  res <- data.frame(x=xfit, y=y, yfit = yfit)
  return(res)
}

plot.fit_curvature <- function(obj, y, res) {
  x <- 1:length(y)
  plot(x, y, col=ifelse(x==res$x, "red", "black"))   
}

fit_curvature_min <- function() {
  obj <- fit_curvature()
  obj$func <- min
  class(obj) <- append("fit_curvature_min", class(obj))    
  return(obj)
}

fit_curvature_max <- function() {
  obj <- fit_curvature()
  obj$func <- max
  class(obj) <- append("fit_curvature_max", class(obj))    
  return(obj)
}

### smoothing

smoothing <- function(n) {
  obj <- dal_transform()
  obj$n <- n
  class(obj) <- append("smoothing", class(obj))    
  return(obj)
}

prepare.smoothing <- function(obj, data) {
  v <- data
  interval <- obj$interval
  names(interval) <- NULL
  interval[1] <- min(v)
  interval[length(interval)] <- max(v)
  interval.adj <- interval
  interval.adj[1] <- -.Machine$double.xmax
  interval.adj[length(interval)] <- .Machine$double.xmax  
  obj$interval <- interval
  obj$interval.adj <- interval.adj
  return(obj)
}

action.smoothing <- function(obj, data) {
  v <- data
  interval.adj <- obj$interval.adj
  vp <- cut(v, unique(interval.adj), FALSE, include.lowest=TRUE)
  m <- tapply(v, vp, mean)
  vm <- m[vp]
  return(vm)  
}

optimize.smoothing <- function(obj, data, do_plot=FALSE) {
  n <- obj$n
  opt <- data.frame()
  interval <- list()
  for (i in 1:n)
  {
    obj$n <- i
    obj <- prepare(obj, data)
    vm <- action(obj, data)
    mse <- mean((data - vm)^2, na.rm = TRUE) 
    row <- c(mse , i)
    opt <- rbind(opt, row)
  }
  colnames(opt)<-c("mean","num") 
  curv <- fit_curvature_max()
  res <- action(curv, opt$mean)
  obj$n <- res$x
  if (do_plot)
    plot(curv, y=opt$mean, res)
  return(obj)
}

# smoothing by interval
smoothing_inter <- function(n) {
  obj <- smoothing(n)
  class(obj) <- append("smoothing_inter", class(obj))    
  return(obj)  
}

prepare.smoothing_inter <- function(obj, data) {
  v <- data
  n <- obj$n
  bp <- boxplot(v, range=1.5, plot = FALSE)
  bimax <- bp$stats[5]
  bimin <- bp$stats[1]
  if (bimin == bimax) {
    bimax = max(v)
    bimin = min(v)
  }
  obj$interval <- seq(from = bimin, to = bimax, by = (bimax-bimin)/n)
  obj <- prepare.smoothing(obj, data)
  return(obj)
}

# smoothing by freq
smoothing_freq <- function(n) {
  obj <- smoothing(n)
  class(obj) <- append("smoothing_freq", class(obj))    
  return(obj)  
}

prepare.smoothing_freq <- function(obj, data) {
  v <- data
  n <- obj$n
  p <- seq(from = 0, to = 1, by = 1/n)
  obj$interval <- quantile(v, p)
  obj <- prepare.smoothing(obj, data)
  return(obj)
}

# smoothing by cluster
smoothing_cluster <- function(n) {
  obj <- smoothing(n)
  class(obj) <- append("smoothing_cluster", class(obj))    
  return(obj)  
}

prepare.smoothing_cluster <- function(obj, data) {
  v <- data
  n <- obj$n
  km <- kmeans(x = v, centers = n)
  s <- sort(km$centers)
  s <- stats::filter(s,rep(1/2,2), sides=2)[1:(n-1)]
  obj$interval <- c(min(v), s, max(v))
  obj <- prepare.smoothing(obj, data)
  return(obj)
}

smoothing_evaluation <- function(data, attribute) {
  obj <- list(data=as.factor(data), attribute=as.factor(attribute))
  attr(obj, "class") <- "cluster_evaluation"  
  
  library(dplyr)
  
  compute_entropy <- function(obj) {
    value <- getOption("dplyr.summarise.inform")
    options(dplyr.summarise.inform = FALSE)
    
    base <- data.frame(x = obj$data, y = obj$attribute) 
    tbl <- base %>% group_by(x, y) %>% summarise(qtd=n()) 
    tbs <- base %>% group_by(x) %>% summarise(t=n()) 
    tbl <- merge(x=tbl, y=tbs, by.x="x", by.y="x")
    tbl$e <- -(tbl$qtd/tbl$t)*log(tbl$qtd/tbl$t,2)
    tbl <- tbl %>% group_by(x) %>% summarise(ce=sum(e), qtd=sum(qtd)) 
    tbl$ceg <- tbl$ce*tbl$qtd/length(obj$data)
    obj$entropy_clusters <- tbl
    obj$entropy <- sum(obj$entropy$ceg)
    
    options(dplyr.summarise.inform = value)
    return(obj)
  }
  obj <- compute_entropy(obj)
  return(obj)
}

### PCA

# Data Transformation

# PCA

dt_pca <- function(attribute=NULL) {
  obj <- dal_transform()
  obj$attribute <- attribute
  class(obj) <- append("dt_pca", class(obj))    
  return(obj)
}  

prepare.dt_pca <- function(obj, data) {
  data <- data.frame(data)
  attribute <- obj$attribute
  if (!is.null(attribute)) {
    data[,attribute] <- NULL
  }
  nums <- unlist(lapply(data, is.numeric))
  remove <- NULL
  for(j in names(nums[nums])) {
    if(min(data[,j])==max(data[,j]))
      remove <- cbind(remove, j)
  }
  nums[remove] <- FALSE
  data = as.matrix(data[ , nums])
  
  pca_res <- prcomp(data, center=TRUE, scale.=TRUE)
  y <-  cumsum(pca_res$sdev^2/sum(pca_res$sdev^2))
  curv <-  fit_curvature_min()
  res <- action(curv, y)
  
  obj$pca.transf <- as.matrix(pca_res$rotation[, 1:res$x])
  obj$nums <- nums
  
  return(obj)
}

action.dt_pca <- function(obj, data) {
  attribute <- obj$attribute
  pca.transf <- obj$pca.transf
  nums <- obj$nums
  
  data <- data.frame(data)
  if (!is.null(attribute)) {
    predictand <- data[,attribute]
    data[,attribute] <- NULL
  }
  data = as.matrix(data[ , nums])
  
  data = data %*% pca.transf
  data = data.frame(data)
  if (!is.null(attribute)){
    data[,attribute] <- predictand
  }
  return(data) 
}  

### Outliers

outliers <- function(alpha = 1.5) {
  obj <- dal_transform()
  obj$alpha <- alpha
  class(obj) <- append("outliers", class(obj))    
  return(obj)
}

prepare.outliers <- function(obj, data) {
  lq1 <- NA
  hq3 <- NA
  if(is.matrix(data) || is.data.frame(data)) {
    lq1 <- rep(NA, ncol(data))
    hq3 <- rep(NA, ncol(data))
    if (nrow(data) >= 30) {
      for (i in 1:ncol(data)) {
        if (is.numeric(data[,i])) {
          q <- quantile(data[,i])
          IQR <- q[4] - q[2]
          lq1[i] <- q[2] - obj$alpha*IQR
          hq3[i] <- q[4] + obj$alpha*IQR
        }
      }
    }
  }
  else {
    if ((length(data) >= 30) && is.numeric(data)) {
      q <- quantile(data)
      IQR <- q[4] - q[2]
      lq1 <- q[2] - obj$alpha*IQR
      hq3 <- q[4] + obj$alpha*IQR
    }
  } 
  obj$lq1 <- lq1
  obj$hq3 <- hq3
  return(obj)
}

action.outliers <- function(obj, data)
{
  idx <- FALSE
  lq1 <- obj$lq1
  hq3 <- obj$hq3
  if (is.matrix(data) || is.data.frame(data)) {
    idx = rep(FALSE, nrow(data))
    for (i in 1:ncol(data)) 
      if (!is.na(lq1[i]) && !is.na(hq3[i]))
        idx = idx | (!is.na(data[,i]) & (data[,i] < lq1[i] | data[,i] > hq3[i]))
  }
  if(is.matrix(data))
    data <- adjust.matrix(data[!idx,])
  else if (is.data.frame(data))
    data <- adjust.data.frame(data[!idx,])
  else {
    if (!is.na(lq1) && !is.na(hq3)) {
      idx <- data < lq1 | data > hq3
      data <- data[!idx]
    }
    else
      idx <- rep(FALSE, length(data))
  }
  attr(data, "idx") <- idx
  return(data)
}

### Normalization

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
ts_normalize <- function() {
  obj <- normalize()
  
  class(obj) <- append("ts_normalize", class(obj))    
  return(obj)
}

# ts_gminmax
ts_gminmax <- function() {
  obj <- ts_normalize()
  class(obj) <- append("ts_gminmax", class(obj))    
  return(obj)
}

prepare.ts_gminmax <- function(obj, data) {
  out <- outliers()
  out <- prepare(out, data)
  data <- action(out, data)
  
  obj$gmin <- min(data)
  obj$gmax <- max(data)
  
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
ts_gminmax_diff <- function() {
  obj <- ts_normalize()
  class(obj) <- append("ts_gminmax_diff", class(obj))    
  return(obj)
}

prepare.ts_gminmax_diff <- function(obj, data) {
  data <- data[,2:ncol(data)]-data[,1:(ncol(data)-1)]
  obj <- prepare.ts_gminmax(obj, data)
  return(obj)
}

action.ts_gminmax_diff <- function(obj, data, x=NULL) {
  if (!is.null(x)) {
    ref <- attr(data, "ref")
    sw <- attr(data, "sw")
    x <- x-ref
    x <- (x-obj$gmin)/(obj$gmax-obj$gmin)
    return(x)
  }
  else {
    ref <- as.vector(data[,ncol(data)])
    cnames <- colnames(data)
    for (i in (ncol(data)-1):1)
      data[,i+1] <- data[, i+1] - data[,i]
    data <- data[,2:ncol(data)]
    data <- (data-obj$gmin)/(obj$gmax-obj$gmin)
    attr(data, "ref") <- ref
    attr(data, "sw") <- ncol(data)
    attr(data, "cnames") <- cnames
    return(data)
  }
}

deaction.ts_gminmax_diff <- function(obj, data, x=NULL) {
  cnames <- attr(data, "cnames")
  ref <- attr(data, "ref")
  sw <- attr(data, "sw")
  if (!is.null(x)) {
    x <- x * (obj$gmax-obj$gmin) + obj$gmin
    x <- x + ref
    return(x)
  }
  else {
    data <- data * (obj$gmax-obj$gmin) + obj$gmin
    data <- cbind(data, ref)
    for (i in (ncol(data)-1):1)
      data[,i] <- data[, i+1] - data[,i]
    colnames(data) <- cnames
    attr(data, "ref") <- ref
    attr(data, "sw") <- ncol(data)
    attr(data, "cnames") <- cnames
    return(data)
  }
}

#ts_swminmax
ts_swminmax <- function() {
  obj <- ts_normalize()
  class(obj) <- append("ts_swminmax", class(obj))    
  return(obj)
}

prepare.ts_swminmax <- function(obj, data) {
  out <- outliers()
  out <- prepare(out, data)
  data <- action(out, data)
  return(obj)
}

action.ts_swminmax <- function(obj, data, x=NULL) {
  if (!is.null(x)) {
    i_min <- attr(data, "i_min")
    i_max <- attr(data, "i_max")
    x <- (x-i_min)/(i_max-i_min)
    return(x)
  }
  else {
    i_min <- apply(data, 1, min)
    i_max <- apply(data, 1, max)
    data <- (data-i_min)/(i_max-i_min)
    attr(data, "i_min") <- i_min
    attr(data, "i_max") <- i_max
    return(data)
  }
}

deaction.ts_swminmax <- function(obj, data, x=NULL) {
  i_min <- attr(data, "i_min")
  i_max <- attr(data, "i_max")
  if (!is.null(x)) {
    x <- x * (i_max - i_min) + i_min
    return(x)
  }
  else {
    data <- data * (i_max - i_min) + i_min
    attr(data, "i_min") <- i_min
    attr(data, "i_max") <- i_max
    return(data)
  }
}

#ts_an
ts_an <- function() {
  obj <- ts_normalize()
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





