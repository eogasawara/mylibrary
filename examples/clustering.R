#setwd("C:/Users/eduar/OneDrive - cefet-rj.br/Git/mylibrary")
#source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myclustering.R")
load(url("https://github.com/eogasawara/mylibrary/raw/master/data/wine.RData"))

#pkgs <- c("factoextra",  "NbClust")
#install.packages(pkgs)

cwine <- wine
cwine_class <- cwine$Class
cwine$Class <- NULL
kmeans_w <- clust_kmeans(cwine, 3)
clust_plot(kmeans_w$model, cwine)
ent_w <- clust_entropy(cwine_class, kmeans_w$cluster)
print(ent_w)

wine_mm <- normalize.minmax(cwine)[[1]]
kmeans_mm <- clust_kmeans(wine_mm, 3)
clust_plot(kmeans_mm$model, wine_mm)
ent_mm <- clust_entropy(cwine_class, kmeans_mm$cluster)
print(ent_mm)


kmeans_best <- clust_optimized_kmeans(wine_mm)
fviz_nbclust(wine_mm, kmeans, method = "wss") + 
  geom_vline(xintercept = kmeans_best$cm$x, linetype = 2)+  
  labs(subtitle = "Elbow method")

fviz_nbclust(wine_mm, kmeans, method = "silhouette") + labs(subtitle = "Silhouette method")

fviz_nbclust(wine_mm, kmeans, method = "gap_stat", nboot = 50) + labs(subtitle = "Gap statistic method")

wine_mm <- normalize.minmax(cwine)[[1]]
kmeans_mm <- clust_kmeans(wine_mm, kmeans_best$cm$x)
clust_plot(kmeans_mm$model, wine_mm)
ent_mm <- clust_entropy(cwine_class, kmeans_mm$cluster)
print(ent_mm)

pamk.result <- pamk(wine_mm)
pamk.result$nc
pamk.result$pamobject$medoids
table(pamk.result$pamobject$clustering, cwine_class)

pam3n <- clust_pam(wine_mm, 3)
table(pam3n$cluster, cwine_class)
ent_mm <- clust_entropy(cwine_class, pam3n$cluster)
head(pam3n$data)
print(pam3n$table)
print(pam3n$entropy)

pam_best <- clust_optimized_pam(wine_mm)
fviz_nbclust(wine_mm, pam, method = "wss") + 
  geom_vline(xintercept = pam_best$cm$x, linetype = 2)+  
  labs(subtitle = "Elbow method")

fviz_nbclust(wine_mm, pam, method = "silhouette") + labs(subtitle = "Silhouette method")

fviz_nbclust(wine_mm, pam, method = "gap_stat", nboot = 50) + labs(subtitle = "Gap statistic method")

dbs3n <- clust_dbscan(winen, "X1", eps = 0.425, MinPts = 10)
head(dbs3n$data)
print(dbs3n$table)
print(dbs3n$entropy)
options(repr.plot.width=5, repr.plot.height=4)
plotcluster(winenp, dbs3n$clu$cluster)  

t <- sort(dbscan::kNNdist(winen, k =  10))
cm <- curvature.max(c(1:length(t)),t,do_plot=FALSE)
dbscan::kNNdistplot(winen, k =  10)
abline(h = cm$y, lty = 2)

dbs3n <- clust_dbscan(winen, "X1", eps = cm$y, MinPts = 10)
head(dbs3n$data)
print(dbs3n$table)
print(dbs3n$entropy)
options(repr.plot.width=5, repr.plot.height=4)
plotcluster(winenp, dbs3n$clu$cluster)  

idx <- sample(1:dim(winen)[1], 40)
hc <- hclust(dist(wine_mm), method = "ave")
plot(hc, hang = 5, labels = cwine_class)
rect.hclust(hc, k = 3)
groups <- cutree(hc, k = 3)

hc1 <- hclust(d, method = "complete" )

https://www.r-bloggers.com/how-to-perform-hierarchical-clustering-using-r/
https://www.rdocumentation.org/packages/FSelector/versions/0.31/topics/entropy.based



