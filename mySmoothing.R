source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myGraphic.R")

loadlibrary("caret")
loadlibrary("MASS")
loadlibrary("dplyr")

# smoothing

smoothing <- function(v, interval) {
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
  
  return (list(smoothing=m, bins_factor=vp, bins=vm, mse=mse, interval=interval, interval.adj=interval.adj))
}

# smoothing by interval

smoothing.interval <- function(v, n = NULL, interval=NULL, range=1.5) {
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
  return(smoothing(v,interval))
}

# smoothing by freq

smoothing.freq <- function(v, n = NULL, interval=NULL) {
  if (is.null(interval)) {
    p <- seq(from = 0, to = 1, by = 1/n)
    interval <- quantile(v, p)
  }
  return(smoothing(v,interval))
}

# smoothing by cluster

smoothing.cluster <- function(v, n = NULL, interval=NULL) {
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
  return(smoothing(v,interval))
}

# optimizing smoothing

smoothing.opt <- function(v, smoothing=NULL, n=20, do_plot=FALSE) {
  z <- data.frame()
  interval <- list()
  for (i in 1:n)
  {
    t <- smoothing(v, i)
    interval = append(interval, list(t))
    newrow <- c(t$mse , i)
    z <- rbind(z,newrow)
  }
  colnames(z)<-c("mean","num") 
  res <- curvature.max(z$num, z$mean, do_plot = do_plot)
  return(interval[[res$x]])
}

smoothing_entropy <- function(cluster, class) {
  tbl <- data.frame(x = cluster, y = class) %>% group_by(x, y) %>% summarise(qtd=n()) 
  tbs <- data.frame(x = cluster, y = class) %>% group_by(x) %>% summarise(t=n()) 
  tbl <- merge(x=tbl, y=tbs, by.x="x", by.y="x")
  tbl$e <- -(tbl$qtd/tbl$t)*log(tbl$qtd/tbl$t,2)
  tbl <- tbl %>% group_by(x) %>% summarise(ce=sum(e), qtd=sum(qtd)) 
  tbl$ceg <- tbl$ce*tbl$qtd/length(cluster)
  tbl <- sum(tbl$ceg)
  return (tbl)
}


