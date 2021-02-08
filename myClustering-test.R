# version 1.0
#source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myClustering.R")
#source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myClusteringEvaluation.R")
data(iris)

iris_cluster <- iris[,1:4]
obj <- cluster_kmeans(iris_cluster)
obj <- prepare(obj)
clu <- action(obj)

eval <- cluster_evaluation(clu, iris$Species)
eval <- prepare(eval)
eval_res <- action(eval)
