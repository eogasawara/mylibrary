# version 1.0
#source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myClustering.R")
#source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myClusteringEvaluation.R")
data(iris)

eval <- cluster_evaluation(rep(1, nrow(iris)), iris$Species)
eval <- prepare(eval)
print(action(eval)$entropy)


iris_cluster <- iris[,1:4]


obj <- cluster_kmeans(iris_cluster, k=3)
obj <- prepare(obj)
clu <- action(obj)

eval <- cluster_evaluation(clu, iris$Species)
eval <- prepare(eval)
print(action(eval)$entropy)


obj <- cluster_kmeans(iris_cluster)
obj <- prepare(obj)
clu <- action(obj)

eval <- cluster_evaluation(clu, iris$Species)
eval <- prepare(eval)
print(action(eval)$entropy)


