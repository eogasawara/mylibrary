# version 1.0
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myOutlier.R")

head(iris)
nrow(iris)

out <- outliers(iris)
out <- prepare(out)
table(out$idx)
iris.out <- action(out)

head(iris.out)
nrow(iris.out)


