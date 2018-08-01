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

