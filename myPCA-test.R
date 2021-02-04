# version 1.0
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myPCA.R")

mypca <- dt_pca(iris, "Species")
mypca <- prepare(mypca)
iris.pca <- action(mypca)

head(iris.pca)
head(mypca$transf$pca.transf)

source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myGraphic.R")
loadlibrary("RColorBrewer")
loadlibrary("dplyr")
col.set <- brewer.pal(11, 'Spectral')
mycolors <- col.set[c(1,3,5,7,9)]

grf <- plot.scatter(iris.pca %>% select(x=PC1, value=PC2, variable=Species), colors=mycolors[1:3])
plot(grf)


