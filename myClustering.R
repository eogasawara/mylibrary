# version 1.0
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myFitting.R")

# clustering
clustering <- function(data) {
  obj <- rel_transform(data)

  class(obj) <- append("clustering", class(obj))    
  return(obj)
}

# kmeans
cluster_kmeans <- function(data, k = NULL) {
  obj <- clustering(data)
  obj$k <- k
  
  class(obj) <- append("cluster_kmeans", class(obj))
  return(obj)
}

prepare.cluster_kmeans <- function(obj) {
  loadlibrary("cluster")
  if (is.null(obj$k)) {
    obj <- optimize(obj)
  }
  k <- obj$k
  cluster <- kmeans(x = obj$data, centers = k)
  dist <- 0
  for (i in 1:k) {
    idx <- i==cluster$cluster
    center <- cluster$centers[i,]
    dist <- dist + sum(rowSums((obj$data[idx,] - center)^2))
  }
  obj$dist <- dist
  obj$cluster <- cluster
  return(obj)
}

action.cluster_kmeans <- function(obj) {
  return(obj$cluster$cluster)
}

optimize.cluster_kmeans <- function(obj, kmax=20, do_plot=FALSE) {
  loadlibrary("factoextra")  
  
  t <- fviz_nbclust(obj$data, kmeans, k.max = kmax, method = "wss")
  
  myfit <- fit_curvature_max(t$data$y)
  myfit <- prepare(myfit)
  res <- action(myfit)
  if (do_plot)
    plot(myfit)
  obj$k <- myfit$xfit
  
  return(obj)
}
  
# pam
cluster_pam <- function(data, k = NULL) {
  obj <- clustering(data)
  obj$k <- k
  
  class(obj) <- append("cluster_pam", class(obj))
  return(obj)
}

prepare.cluster_pam <- function(obj) {
  loadlibrary("cluster")
  if (is.null(obj$k)) {
    obj <- optimize(obj)
  }
  k <- obj$k

  cluster <- pam(obj$data, k)
  dist <- 0
  for (i in 1:k) {
    idx <- i==cluster$clustering
    center <- cluster$medoids[i,]
    dist <- dist + sum(rowSums((obj$data[idx,] - center)^2))
  }
  
  obj$dist <- dist
  obj$cluster <- cluster
  return(obj)
}

action.cluster_pam <- function(obj) {
  return(obj$cluster$clustering)
}

optimize.cluster_pam <- function(obj, kmax=20, do_plot=FALSE) {
  loadlibrary("factoextra")  
  t <- fviz_nbclust(obj$data, pam, k.max = kmax, method = "wss")
  
  myfit <- fit_curvature_max(t$data$y)
  myfit <- prepare(myfit)
  res <- action(myfit)
  if (do_plot)
    plot(myfit)
  obj$k <- myfit$xfit
  
  return(obj)
}

# dbscan
cluster_dbscan <- function(data, eps=NULL, MinPts) {
  obj <- clustering(data)
  obj$eps <- eps
  obj$MinPts <- MinPts
  
  class(obj) <- append("cluster_dbscan", class(obj))
  return(obj)
}

prepare.cluster_dbscan <- function(obj) {
  loadlibrary("dbscan")
  if (is.null(obj$eps))
      obj <- optimize(obj)

  cluster <- fpc::dbscan(obj$data, eps = obj$eps, MinPts = obj$MinPts)
  
  obj$cluster <- cluster
  return(obj)
}

action.cluster_dbscan <- function(obj) {
  return(obj$cluster$cluster)
}


optimize.cluster_dbscan <- function(obj, do_plot=FALSE) {
  t <- sort(dbscan::kNNdist(obj$data, k = obj$MinPts))
  
  myfit <- fit_curvature_max(t)
  myfit <- prepare(myfit)
  res <- action(myfit)
  if (do_plot)
    plot(myfit)
  obj$eps <- myfit$y
  
  return(obj)
}


#cluster_evaluation

cluster_evaluation <- function(cluster, attribute) {
  obj <- list(data=cluster, attribute=attribute)
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
