# version 1.0
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myFitting.R")

wine <- get(load(url("https://raw.githubusercontent.com/eogasawara/mylibrary/master/data/wine.RData")))
head(wine)

pca_res = prcomp(wine[,2:ncol(wine)], center=TRUE, scale.=TRUE)
y <- cumsum(pca_res$sdev^2/sum(pca_res$sdev^2))
x <- 1:length(y)
plot(x, y)

myfit <- curvature_min(y)
myfit <- prepare(myfit)
res <- action(myfit)
plot(myfit)

myfit <- curvature_max(-y)
myfit <- prepare(myfit)
res <- action(myfit)
plot(myfit)


