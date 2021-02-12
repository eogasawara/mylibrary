# version 1.0
#source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myClustering.R")
data(iris)

eval <- cluster_evaluation(rep(1, nrow(iris)), iris$Species)
print(eval$entropy)

test_clustering <- function(model, data, attribute) {
  print(class(model)[1])
  
  clu <- action(model, data)
  eval <- cluster_evaluation(clu, attribute)
  print(action(eval)$entropy)
}

test_clustering(cluster_kmeans(k=3), iris[,1:4], iris[,5])

test_clustering(cluster_pam(k=3), iris[,1:4], iris[,5])

test_clustering(cluster_dbscan(eps = 0.4, MinPts = 3), iris[,1:4], iris[,5])

test_clustering(cluster_dbscan(MinPts = 3), iris[,1:4], iris[,5])


