# version 1.0
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myClustering.R")
data(iris)

eval <- cluster_evaluation(rep(1, nrow(iris)), iris$Species)
eval <- prepare(eval)
print(action(eval)$entropy)


test_clustering <- function(model, attribute) {
  print(class(model)[1])
  
  model <- prepare(model)
  clu <- action(model)
  
  eval <- cluster_evaluation(clu, attribute)
  eval <- prepare(eval)
  print(action(eval)$entropy)
}

test_clustering(cluster_kmeans(iris[,1:4], k=3), iris[,5])

test_clustering(cluster_kmeans(iris[,1:4]), iris[,5])

test_clustering(cluster_pam(iris[,1:4]), iris[,5])

test_clustering(cluster_dbscan(iris[,1:4], eps = 0.4, MinPts = 3), iris[,5])

test_clustering(cluster_dbscan(iris[,1:4], MinPts = 3), iris[,5])


