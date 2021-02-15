source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myNormalization.R")
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myPCA.R")
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myGraphic.R")

loadlibrary("RColorBrewer")
loadlibrary("dplyr")
loadlibrary("gridExtra")
loadlibrary("reshape")

col.set <- brewer.pal(11, 'Spectral')
mycolors <- col.set[c(1,10,3)]
head(iris[,1:4])

myiris <- iris[iris$Species!="versicolor",]
norm <- minmax()
norm <- prepare(norm, myiris)
myiris <- action(norm, myiris)
mypca <- dt_pca("Species")
mypca <- prepare(mypca, myiris)
iris.pca <- action(mypca, myiris)
iris.pca$Species <- as.numeric(iris.pca$Species)
iris.pca$Species<- as.factor(iris.pca$Species)
levels(iris.pca$Species) <- c("Y1", "Y2")
grf <- plot.scatter(iris.pca %>% dplyr::select(x=PC1, value=PC2, variable=Species), colors=mycolors[1:2], label_x="(a) initial data")
grf <- grf + geom_abline(intercept = 0.1, slope = -1.1, color=mycolors[3], linetype="dashed", size=1)
grfA <- grf + theme(legend.position = c(0.5, 0.2))

myiris <- iris[iris$Species!="versicolor",]
myiris <- myiris[1:nrow(myiris) > 20,]
norm <- minmax()
norm <- prepare(norm, myiris)
myiris <- action(norm, myiris)
mypca <- dt_pca("Species")
mypca <- prepare(mypca, myiris)
iris.pca <- action(mypca, myiris)
iris.pca$Species <- as.numeric(iris.pca$Species)
iris.pca$Species<- as.factor(iris.pca$Species)
levels(iris.pca$Species) <- c("Y1", "Y2")
grf <- plot.scatter(iris.pca %>% dplyr::select(x=PC1, value=PC2, variable=Species), colors=mycolors[1:2], label_x="(b) virtual concept drift")
grf <- grf + geom_abline(intercept = 1.1, slope = -1.1, color=mycolors[3], linetype="dashed", size=1)
grfB <- grf + theme(legend.position = "none")

myiris <- iris[iris$Species!="setosa",]
norm <- minmax()
norm <- prepare(norm, myiris)
myiris <- action(norm, myiris)
mypca <- dt_pca("Species")
mypca <- prepare(mypca, myiris)
iris.pca <- action(mypca, myiris)
iris.pca$Species <- as.numeric(iris.pca$Species)
iris.pca$Species<- as.factor(iris.pca$Species)
levels(iris.pca$Species) <- c("Y1", "Y2")
grf <- plot.scatter(iris.pca %>% dplyr::select(x=PC1, value=PC2, variable=Species), colors=mycolors[2:1], label_x="(c) real concept data")
grf <- grf + geom_abline(intercept = -1, slope = -1.1, color=mycolors[3], linetype="dashed", size=1)
grfC <- grf + theme(legend.position = "none")

grid.arrange(grfA, grfB, grfC, ncol=3)