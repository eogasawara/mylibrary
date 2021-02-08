source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myPreprocessing.R")
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myFeature.R")
loadlibrary("useful")
loadlibrary("dbscan")
loadlibrary("fpc")
loadlibrary("cluster")
loadlibrary("DescTools")
loadlibrary("factoextra")
loadlibrary("NbClust")

clust_entropy <- function(class, cluster) {
  class <- as.factor(class)
  l <- split(class, cluster)
  entropy <- c(1:(length(l)+1))
  entropy[1] <- (Entropy(table(class), base=exp(1)))
  ctable <- table(class)
  
  for (i in 1:length(l)) {
    x <- factor(l[[i]], levels(class))
    entropy[i+1] <- Entropy(table(x), base=exp(1))
    ctable <- rbind(ctable, table(x))
  } 
  return (list(entropy=entropy, table=ctable)) 
}

clust_plot <- function(model, data, class) {
  plot <- plot(model, data=data)
  return(plot)  
}

clust_optimized_kmeans <- function(data, max_k=20) {
  t <- fviz_nbclust(data, kmeans, method = "wss")
  clusters <- data.frame(k=as.integer(t$data$clusters), wss=t$data$y)
  cm <- curvature.max(clusters$k, clusters$wss, do_plot=FALSE)
  return(list(clusters=clusters, cm=cm))
}

clust_kmeans <- function(data, k)
{
  clu <- kmeans(x = data, centers = k)
  dist <- 0
  for (i in 1:k) {
    idx <- i==clu$cluster
    center <- clu$centers[i,]
    dist <- dist + sum(rowSums((data[idx,] - center)^2))
  }
  return (list(data=data, cluster=clu$cluster, model=clu, centers=clu$centers, dist=dist)) 
}

clust_optimized_pam <- function(data, max_k=20) {
  t <- fviz_nbclust(data, pam, method = "wss")
  clusters <- data.frame(k=as.integer(t$data$clusters), wss=t$data$y)
  cm <- curvature.max(clusters$k, clusters$wss, do_plot=FALSE)
  return(list(clusters=clusters, cm=cm))
}

loadlibrary("useful")
loadlibrary("dbscan")
loadlibrary("fpc")
loadlibrary("cluster")
loadlibrary("DescTools")
loadlibrary("factoextra")
loadlibrary("NbClust")
clust_pam <- function(data, k)
{
  clu <- pam(data, k)
  dist <- 0
  for (i in 1:k) {
    idx <- i==clu$clustering
    center <- clu$medoids[i,]
    dist <- dist + sum(rowSums((data[idx,] - center)^2))
  }
  return (list(data=data, cluster=clu$clustering, model=clu, centers=clu$medoids)) 
}


clust_dbscan <- function(data, clabel, eps, MinPts)
{
  predictors_name  = setdiff(colnames(data), clabel)
  data_predictors = data[,predictors_name] 
  data[,clabel] = as.factor(data[,clabel])
  
  clu <- fpc::dbscan(data_predictors, eps = eps, MinPts = MinPts)
  data$cluster = clu$cluster
  
  l = split(data[,clabel], data$cluster)
  entropy <- c(1:(length(l)+1))
  entropy[1] = (Entropy(table(data[,clabel]), base=exp(1)))
  ctable = table(data[,clabel])
  for (i in 1:length(l)) {
    x <- factor(l[[i]], levels(data[,clabel]))
    ctable = rbind(ctable, table(x))
    entropy[i+1] = Entropy(table(x), base=exp(1))
  }  
  
  return (list(data=data, cluster=clu, table=ctable, entropy=entropy)) 
}

