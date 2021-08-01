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


