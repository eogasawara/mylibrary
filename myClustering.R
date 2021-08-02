# version 1.2
# depends myBasic.R
# depends myPreprocessing.R

if (!exists("repos_name"))
  repos_name <<- getOption("repos")[1]

setrepos <- function(repos=repos) {
  repos_name <<- repos 
}

loadlibrary <- function(packagename) 
{
  if (!require(packagename, character.only = TRUE))
  {
    install.packages(packagename, repos=repos_name, dep=TRUE, verbose = FALSE)
    require(packagename, character.only = TRUE)
  }
}

# clustering
clustering <- function() {
  obj <- list()
  attr(obj, "class") <- "clustering"  
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
  myfit <- fit_curvature_max()
  res <- action(myfit, y)
  if (do_plot)
    plot(myfit, y, res)
  obj$k <- res$x

  return(obj)
}

cluster.cluster_kmeans <- function(obj, data) {
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
  myfit <- fit_curvature_max()
  res <- action(myfit, y)
  if (do_plot)
    plot(myfit, y, res)
  obj$k <- res$x
  
  return(obj)
}

cluster.cluster_pam <- function(obj, data) {
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

cluster.cluster_dbscan <- function(obj, data) {
  loadlibrary("fpc")
  
  cluster <- fpc::dbscan(data, eps = obj$eps, MinPts = obj$MinPts)
  
  cluster <- cluster$cluster
  attr(cluster, "dist") <- 0
  return(cluster)
}

optimize.cluster_dbscan <- function(obj, data, do_plot=FALSE) {
  loadlibrary("Rcpp")
  loadlibrary("dbscan")
  t <- sort(dbscan::kNNdist(data, k = obj$MinPts))
  
  y <- t
  myfit <- fit_curvature_max()
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
