
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myOutlier.R")

head(iris)
nrow(iris)

obj <- outliers(iris)
table(obj$idx)

iris_out <- action(obj)

head(iris_out)
nrow(iris_out)


