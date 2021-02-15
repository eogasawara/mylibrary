# version 1.0
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myOutlier.R")

head(iris)
nrow(iris)

#outlier of a dataset
analyze_outliers <- function(obj, data) {
  obj <- prepare(obj, iris)
  data <- action(obj, iris)
  idx <- attr(data, "idx")
  print(table(idx))
  print(head(data))
  print(nrow(data))
}

analyze_outliers(outliers(), iris)
analyze_outliers(outliers(), iris$Sepal.Width)

