# version 1.0
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myClustering.R")
data(iris)

kmeans2 <- clust_kmeans(wine, "X1", 2)
head(kmeans2$data)
options(repr.plot.width=4, repr.plot.height=3)
plot(kmeans2$plot)
print(kmeans2$table)
print(kmeans2$entropy)
