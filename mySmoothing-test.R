# version 1.0
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/mySmoothing.R")
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myClustering.R")

explore_smoothing <- function(obj, data) {
  obj <- prepare(obj, data)
  sl.bi <- action(obj, data)
  print(table(sl.bi))
}

optimize_smoothing <- function(obj, data) {
  obj <- optimize(obj, data, do_plot=TRUE)
  explore_smoothing(obj, data)
}

explore_smoothing(smoothing_inter(n=2), iris$Sepal.Length)
explore_smoothing(smoothing_freq(n=2), iris$Sepal.Length)
explore_smoothing(smoothing_cluster(n=2), iris$Sepal.Length)
optimize_smoothing(smoothing_inter(n=20), iris$Sepal.Length)
optimize_smoothing(smoothing_freq(n=20), iris$Sepal.Length)
optimize_smoothing(smoothing_freq(n=20), iris$Sepal.Length)

#entro <- cluster_evaluation(names(sl.bi),iris$Species)
#entro <- prepare(entro)
#res <- action(entro)
