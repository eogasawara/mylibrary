source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myTSRegression.R")
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myOutlier.R")
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myNormalization.R")

load_series <- function(name) {
  link <- url(sprintf("https://raw.githubusercontent.com/eogasawara/mylibrary/master/data/time-series/%s.RData", name))
  x <- get(load(link))
  return(x)  
}


ts.anomalies.boxplot <- function(data)
{
  obj <- outliers()
  obj <- prepare(obj, data)
  data <- action(obj, data)
  idx <- attr(data, "idx")
  return (idx)
}


ts.anomalies.an <- function(x, k) {
  ts <- ts_data(x, k)
  ma <- apply(ts, 1, mean)
  sxd <- ts - ma
  iF <- ts.anomalies.boxplot(sxd)
  iF <- c(rep(NA, k-1), iF)
  
  ts <- ts_data(rev(x), k)
  ma <- apply(ts, 1, mean)
  sxd <- ts - ma
  iB <- ts.anomalies.boxplot(sxd)
  iB <- c(rep(NA, k-1), iB)
  iB <- rev(iB)
  
  
  i <- iF & iB
  i[1:k] <- iB[1:k]
  i[(length(x)-k+1):length(x)] <- iF[(length(x)-k+1):length(x)]
  return(i)
}

ts.anomalies.kmeans <- function(x, k=3) {
  loadlibrary("cluster")
  sx <- ts_data(x, k)
  pos <- rep(FALSE, nrow(sx))
  clu <- kmeans(x = sx, centers = 1)
  clud  <- rowSums(sx - clu$centers[clu$cluster,])^2
  bp <- ts.anomalies.boxplot(clud)
  bp <- c(rep(FALSE, k-1), bp)
  return(bp)
}

ts.anomalies.dbscan <- function(x, k=3)
{
  loadlibrary("fpc")
  sx <- ts_data(x, k)
  
  eps <- mean(abs(sx[,ncol(sx)] - sx[,ncol(sx)-1]))
  MinPts <- 3 * 2
  
  clu <- fpc::dbscan(sx, eps = eps, MinPts = MinPts)
  bp <- clu$cluster == 0
  bp <- c(rep(FALSE, k-1), bp)
  return(bp)
}


ts.anomalies.nnet <- function(x, k, input_size)
{
  ts <- ts_data(x, k)
  io <- ts_projection(ts)
  model <- tsreg_mlp(ts_gminmax(), input_size=input_size)  
  model <- prepare(model, x=io$input, y=io$output)
  adjust <- action(model, io$input)
  bp <- ts.anomalies.boxplot((io$output-adjust)^2)
  bp <- c(rep(FALSE, k-1), bp)
  return(bp)
}

x <- load_series("exemplo")
plot(x)


teste1 <- function(x) {
  plot(x)
  t <- ts.anomalies.an(x, 12)
  print((1:length(x))[t])
  
  t <- ts.anomalies.kmeans(x)
  print((1:length(x))[t])
  
  t <- ts.anomalies.dbscan(x)
  print((1:length(x))[t])
  
  t <- ts.anomalies.nnet(x, 12, 5)
  print((1:length(x))[t])
}

