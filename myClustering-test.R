# version 1.2
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myBasic.R")
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myPreprocessing.R")
#source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myClustering.R")


data(iris)

eval <- cluster_evaluation(rep(1, nrow(iris)), iris$Species)
print(eval$entropy)

test_clustering <- function(model, data, attribute, opt=FALSE) {
  print(class(model)[1])
  if (opt) 
    model <- optimize(model, data)    
  clu <- action(model, data)
  print(table(clu))
  eval <- cluster_evaluation(clu, attribute)
  print(eval$entropy)
}

test_clustering(cluster_kmeans(k=3), iris[,1:4], iris[,5])

test_clustering(cluster_pam(k=3), iris[,1:4], iris[,5])

test_clustering(cluster_dbscan(eps = 0.4, MinPts = 3), iris[,1:4], iris[,5])

test_clustering(cluster_kmeans(NULL), iris[,1:4], iris[,5], TRUE)

test_clustering(cluster_pam(NULL), iris[,1:4], iris[,5], TRUE)

test_clustering(cluster_dbscan(eps = NULL, MinPts = 3), iris[,1:4], iris[,5], TRUE)


