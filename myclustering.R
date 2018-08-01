source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myPreprocessing.R")
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myFeature.R")
loadlibrary("useful")
loadlibrary("fpc")
loadlibrary("cluster")
loadlibrary("DescTools")

clust_kmeans <- function(data, clabel, k)
{
  predictors_name  = setdiff(colnames(data), clabel)
  data_predictors = data[,predictors_name] 
  data[,clabel] = as.factor(data[,clabel])
  
  clu = kmeans(x = data_predictors, centers = k)
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
  return (list(data=data, cluster=clu, table=ctable, entropy=entropy, plot=plot(clu, data=data_predictors))) 
}

clust_kmeans_best <- function(data, maxk=20) {
  x = c(1:maxk) 
  y = rep(0,maxk)
  k = 1
  for (k in 1:maxk) {
    clu = kmeans(x = data, centers = k)
    for (i in 1:k) {
      mdata = data
      mdata$cluster = clu$cluster
      mdata = filter(mdata, cluster == i)
      mdata$cluster = NULL
      y[k] <- y[k] + sum(sqrt(rowSums(mdata - clu$centers[i,]) ^ 2))
    }
  }
  return (list(x=x, y=y))
}

clust_dbscan <- function(data, clabel, eps, MinPts)
{
  predictors_name  = setdiff(colnames(data), clabel)
  data_predictors = data[,predictors_name] 
  data[,clabel] = as.factor(data[,clabel])
  
  clu <- dbscan(data_predictors, eps = eps, MinPts = MinPts)
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

clust_pam <- function(data, clabel, k)
{
  predictors_name  = setdiff(colnames(data), clabel)
  data_predictors = data[,predictors_name] 
  data[,clabel] = as.factor(data[,clabel])
  
  clu <- pam(data_predictors, k)
  data$cluster = clu$clustering
  
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

