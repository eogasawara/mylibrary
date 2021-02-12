# version 1.0
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myOutlier.R")

head(iris)
nrow(iris)

#outlier of a dataset
out <- outliers()
out <- prepare(out, iris)
iris.out <- action(out, iris)
table(out$idx)
head(iris.out)
nrow(iris.out)


#outlier of an attribute
out <- outliers()
out <- prepare(out, iris$Sepal.Width)
iris.out <- action(out, iris$Sepal.Width)
table(out$idx)
head(iris.out)
length(iris.out)


