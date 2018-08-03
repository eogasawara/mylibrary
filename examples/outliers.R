source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myclustering.R")
load(url("https://github.com/eogasawara/mylibrary/raw/master/data/iris.RData"))


#outliers by boxplot
out <- outliers.boxplot(iris)
myiris <- iris[!out,]
head(iris[out,])
boutliers <- which(out==TRUE)
print(boutliers)

t <- sort(dbscan::kNNdist(irisn, k =  10))
cm <- curvature.max(c(1:length(t)),t, do_plot=FALSE)
dbscan::kNNdistplot(irisn, k =  10)
abline(h = cm$y, lty = 2)
dbs3n <- fpc::dbscan(irisn, eps = cm$y, MinPts = 10)
irisn$cluster <- dbs3n$cluster
dbsoutliers = which(irisn$cluster == 0)
print(dbsoutliers)


irisn <- iris
irisn$Species <- NULL
outlier.scores <- lofactor(irisn, k=5)
plot(density(outlier.scores))
t <- sort(outlier.scores, decreasing=T)
plot(t)
looutliers <- order(outlier.scores, decreasing=T)[1:5]
print(looutliers)


n <- nrow(irisn)
labels <- 1:n
labels[-looutliers] <- "."
biplot(prcomp(irisn), cex=.8, xlabs=labels)

pch <- rep(".", n)
pch[looutliers] <- "+"
col <- rep("black", n)
col[looutliers] <- "red"
pairs(irisn, pch=pch, col=col)


mycm <- dt.categ_mapping(iris, "Species")
mycm <- mycm[with(mycm, order(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)),]
mod <- lm(Speciessetosa+Speciesversicolor+Speciesvirginica ~ ., data=mycm)
plot(mod,4)




#http://www.rdatamining.com/examples/outlier-detection