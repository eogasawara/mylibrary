# version 1.0
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/mySmoothing.R")
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myClustering.R")

explore_smoothing <- function(obj, data, attribute) {
  obj <- prepare(obj, data)
  sl.bi <- action(obj, data)
  print(table(sl.bi))
  
  entro <- cluster_evaluation(as.factor(names(sl.bi)), attribute)
  print(entro$entropy)
}

optimize_smoothing <- function(obj, data, attribute) {
  obj <- optimize(obj, data, do_plot=TRUE)
  explore_smoothing(obj, data, attribute)
}

explore_smoothing(smoothing_inter(n=2), iris$Sepal.Length, iris$Species)
explore_smoothing(smoothing_freq(n=2), iris$Sepal.Length, iris$Species)
explore_smoothing(smoothing_cluster(n=2), iris$Sepal.Length, iris$Species)
optimize_smoothing(smoothing_inter(n=20), iris$Sepal.Length, iris$Species)
optimize_smoothing(smoothing_freq(n=20), iris$Sepal.Length, iris$Species)
optimize_smoothing(smoothing_freq(n=20), iris$Sepal.Length, iris$Species)

