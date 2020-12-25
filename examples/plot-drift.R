source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myPreprocessing.R")
loadlibrary("RColorBrewer")
loadlibrary("dplyr")
loadlibrary("gridExtra")
loadlibrary("reshape")

col.set <- brewer.pal(11, 'Spectral')
mycolors <- col.set[c(1,10,3)]
head(iris[,1:4])


myiris <- iris[iris$Species!="versicolor",]
myiris <- normalize.minmax(myiris)$data
mypca <- dt.pca(myiris, "Species")$pca
mypca <- normalize.minmax(mypca)$data
mypca$Species <- as.numeric(mypca$Species)
mypca$Species<- as.factor(mypca$Species)
levels(mypca$Species) <- c("Y1", "Y2")
grf <- plot.scatter(mypca %>% dplyr::select(x=PC1, value=PC2, variable=Species), colors=mycolors[1:2], label_x="(a) initial data")
grf <- grf + geom_abline(intercept = 1, slope = -1, color=mycolors[3], linetype="dashed", size=1)
grfA <- grf + theme(legend.position = c(0.5, 0.2))

myiris <- iris
myiris <- normalize.minmax(myiris)$data
mypca <- dt.pca(myiris, "Species")$pca
mypca <- normalize.minmax(mypca)$data
mypca$Species <- as.numeric(mypca$Species)
mypca$Species[mypca$Species==3] <- 2
mypca$Species<- as.factor(mypca$Species)
levels(mypca$Species) <- c("Y1", "Y2")
grf <- plot.scatter(mypca %>% dplyr::select(x=PC1, value=PC2, variable=Species), colors=mycolors[1:2], label_x="(b) virtual concept drift")
grf <- grf + geom_abline(intercept = 1, slope = -1, color=mycolors[3], linetype="dashed", size=1)
grfB <- grf + theme(legend.position = "none") 



myiris <- iris[iris$Species!="setosa",]
myiris <- normalize.minmax(myiris)$data
mypca <- dt.pca(myiris, "Species")$pca
mypca <- normalize.minmax(mypca)$data
mypca$Species <- as.numeric(mypca$Species)
mypca$Species<- as.factor(mypca$Species)
levels(mypca$Species) <- c("Y1", "Y2")
grf <- plot.scatter(mypca %>% dplyr::select(x=PC1, value=PC2, variable=Species), colors=mycolors[2:1], label_x="(c) real concept drift")
grf <- grf + geom_abline(intercept = 1.1, slope = -1, color=mycolors[3], linetype="dashed", size=1)
grfC <- grf + theme(legend.position = "none") 


grid.arrange(grfA, grfB, grfC, ncol=3)