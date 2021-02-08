# version 1.0
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myClusteringEvaluation.R")

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
  clusters <- data.frame(k=as.integer(t$data$clusters), wss=t$data$y)
  cm <- curvature.max(clusters$k, clusters$wss, do_plot=FALSE)
}
  
