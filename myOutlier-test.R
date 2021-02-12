# version 1.0
#source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myOutlier.R")

head(iris)
nrow(iris)

#outlier of a dataset
analyze_outliers <- function(obj, data) {
  obj <- prepare(obj, iris)
  ouliers <- action(obj, iris)
  idx <- attr(outliers, "idx")
  print(table(idx))
  print(head(ouliers))
  print(nrow(ouliers))
}

analyze_outliers(outliers(), iris)
analyze_outliers(outliers(), iris$Sepal.Width)

