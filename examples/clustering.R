source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myclustering.R")
load(url("https://github.com/eogasawara/mylibrary/raw/master/data/wine.RData"))

kmeans2 <- clust_kmeans(wine, "X1", 2)
head(kmeans2$data)
options(repr.plot.width=4, repr.plot.height=3)
plot(kmeans2$plot)
print(kmeans2$table)
print(kmeans2$entropy)

kmeans3 <- clust_kmeans(wine, "X1", 3)
head(kmeans3$data)
options(repr.plot.width=4, repr.plot.height=3)
plot(kmeans3$plot)
print(kmeans3$table)
print(kmeans3$entropy)

winen <- wine
winen$X1 <- NULL
kmeans_b <- clust_kmeans_best(winen)
options(repr.plot.width=4, repr.plot.height=3)
plot(kmeans_b$x,kmeans_b$y)

kmeans5 <- clust_kmeans(wine, "X1", 5)
head(kmeans5$data)
options(repr.plot.width=4, repr.plot.height=3)
plot(kmeans5$plot)
print(kmeans5$table)
print(kmeans5$entropy)

winen <- wine
winen$X1 <- NULL
winen = normalize.minmax(winen)[[1]]
winenp <- winen
winen$X1 <- wine$X1

kmeans3n <- clust_kmeans(winen, "X1", 3)
head(kmeans3n$data)
options(repr.plot.width=4, repr.plot.height=3)
plot(kmeans3n$plot)
print(kmeans3n$table)
print(kmeans3n$entropy)

kmeans_b <- clust_kmeans_best(winenp)
options(repr.plot.width=4, repr.plot.height=3)
plot(kmeans_b$x,kmeans_b$y)

kmeans4n <- clust_kmeans(winen, "X1", 4)
head(kmeans4n$data)
options(repr.plot.width=4, repr.plot.height=3)
plot(kmeans4n$plot)
print(kmeans4n$table)
print(kmeans4n$entropy)


pamk.result <- pamk(winenp)
pamk.result$nc
table(pamk.result$pamobject$clustering, winen$X1)

pam3n <- clust_pam(winen, "X1", 3)
head(pam3n$data)
print(pam3n$table)
print(pam3n$entropy)


dbs3n <- clust_dbscan(winen, "X1", eps = 0.425, MinPts = 10)
head(dbs3n$data)
print(dbs3n$table)
print(dbs3n$entropy)
options(repr.plot.width=5, repr.plot.height=4)
plotcluster(winenp, dbs3n$clu$cluster)  

idx <- sample(1:dim(winen)[1], 40)
winens <- winen[idx, ]
hc <- hclust(dist(winens), method = "ave")
plot(hc, hang = -1, labels = winen$X1[idx])
rect.hclust(hc, k = 3)
groups <- cutree(hc, k = 3)
