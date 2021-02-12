# version 1.0
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myFitting.R")

# clustering
clustering <- function() {
  obj <- dal_transform()
  class(obj) <- append("clustering", class(obj))    
  return(obj)
}

# kmeans
cluster_kmeans <- function(k) {
  obj <- clustering()
  obj$k <- k
  
  class(obj) <- append("cluster_kmeans", class(obj))
  return(obj)
}

optimize.cluster_kmeans <- function(obj, data, kmax=20, do_plot=FALSE) {
  loadlibrary("factoextra")  
  
  t <- fviz_nbclust(data, kmeans, k.max = kmax, method = "wss")
  
  y <- t$data$y
  myfit <- fit_curvature_max(y)
  res <- action(myfit, y)
  if (do_plot)
    plot(myfit, y, res)
  obj$k <- res$x

  return(obj)
}

action.cluster_kmeans <- function(obj, data) {
  loadlibrary("cluster")
  k <- obj$k
  cluster <- kmeans(x = data, centers = k)
  dist <- 0
  for (i in 1:k) {
    idx <- i == cluster$cluster
    center <- cluster$centers[i,]
    dist <- dist + sum(rowSums((data[idx,] - center)^2))
  }
  
  cluster <- cluster$cluster
  attr(cluster, "dist") <- dist
  return(cluster)
}

  
# pam
cluster_pam <- function(k) {
  obj <- clustering()
  obj$k <- k
  
  class(obj) <- append("cluster_pam", class(obj))
  return(obj)
}

optimize.cluster_pam <- function(obj, data, kmax=20, do_plot=FALSE) {
  loadlibrary("factoextra")  
  t <- fviz_nbclust(data, pam, k.max = kmax, method = "wss")
  
  y <- t$data$y
  myfit <- fit_curvature_max(y)
  res <- action(myfit, y)
  if (do_plot)
    plot(myfit, y, res)
  obj$k <- res$x
  
  return(obj)
}

action.cluster_pam <- function(obj, data) {
  loadlibrary("cluster")
  cluster <- pam(data, obj$k)
  dist <- 0
  for (i in 1:obj$k) {
    idx <- i==cluster$clustering
    center <- cluster$medoids[i,]
    dist <- dist + sum(rowSums((data[idx,] - center)^2))
  }
  
  cluster <- cluster$cluster
  attr(cluster, "dist") <- dist
  return(cluster)
}

# dbscan
cluster_dbscan <- function(eps, MinPts) {
  obj <- clustering()
  obj$eps <- eps
  obj$MinPts <- MinPts
  
  class(obj) <- append("cluster_dbscan", class(obj))
  return(obj)
}

action.cluster_dbscan <- function(obj, data) {
  loadlibrary("dbscan")
  
  cluster <- fpc::dbscan(data, eps = obj$eps, MinPts = obj$MinPts)
  
  cluster <- cluster$cluster
  attr(cluster, "dist") <- 0
  return(cluster)
}

optimize.cluster_dbscan <- function(obj, data, do_plot=FALSE) {
  t <- sort(dbscan::kNNdist(data, k = obj$MinPts))
  
  y <- t
  myfit <- fit_curvature_max(y)
  res <- action(myfit, y)
  if (do_plot)
    plot(myfit, y, res)
  obj$eps <- res$y
  
  return(obj)
}

#cluster_evaluation

cluster_evaluation <- function(cluster, attribute) {
  obj <- list(data=as.factor(cluster), attribute=as.factor(attribute))
  attr(obj, "class") <- "cluster_evaluation"  

  loadlibrary("dplyr")
  
  compute_entropy <- function(obj) {
    base <- data.frame(x = obj$data, y = obj$attribute) 
    tbl <- base %>% group_by(x, y) %>% summarise(qtd=n()) 
    tbs <- base %>% group_by(x) %>% summarise(t=n()) 
    tbl <- merge(x=tbl, y=tbs, by.x="x", by.y="x")
    tbl$e <- -(tbl$qtd/tbl$t)*log(tbl$qtd/tbl$t,2)
    tbl <- tbl %>% group_by(x) %>% summarise(ce=sum(e), qtd=sum(qtd)) 
    tbl$ceg <- tbl$ce*tbl$qtd/length(obj$data)
    obj$entropy_clusters <- tbl
    obj$entropy <- sum(obj$entropy$ceg)
    return(obj)
  }
  obj <- compute_entropy(obj)
  return(obj)
}
