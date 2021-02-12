# version 1.0
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myTransform.R")
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myFitting.R")

# smoothing
smoothing <- function() {
  obj <- dal_transform()
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
    obj <- prepare(obj)
    vm <- action(obj)
    mse <- mean((data - vm)^2, na.rm = TRUE) 
    row <- c(mse , i)
    opt <- rbind(opt, row)
  }
  colnames(opt)<-c("mean","num") 
  curv <- fit_curvature_max(opt$mean)
  curv <- prepare(curv)
  res <- action(curv)
  obj$n <- res$x
  if (do_plot)
    plot(curv)
  return(obj)
}

# smoothing by interval
smoothing_inter <- function(n) {
  obj <- smoothing()
  obj$n <- n
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
  obj <- prepare.smoothing(obj)
  return(obj)
}

# smoothing by freq
smoothing_freq <- function(n) {
  obj <- smoothing()
  obj$n <- n
  class(obj) <- append("smoothing_freq", class(obj))    
  return(obj)  
}

prepare.smoothing_freq <- function(obj, data) {
  v <- data
  n <- obj$n
  p <- seq(from = 0, to = 1, by = 1/n)
  obj$interval <- quantile(v, p)
  obj <- prepare.smoothing(obj)
  return(obj)
}

# smoothing by cluster
smoothing_cluster <- function(n) {
  obj <- smoothing()
  obj$n <- n
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
  obj <- prepare.smoothing(obj)
  return(obj)
}

