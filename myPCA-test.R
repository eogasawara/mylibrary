# version 1.0
#source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myPCA.R")

mypca <- dt_pca("Species")
mypca <- prepare(mypca, iris)
iris.pca <- action(mypca, iris)

print(head(iris.pca))
print(head(mypca$pca.transf))

source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myGraphic.R")
loadlibrary("RColorBrewer")
loadlibrary("dplyr")
col.set <- brewer.pal(11, 'Spectral')
mycolors <- col.set[c(1,3,5,7,9)]

grf <- plot.scatter(iris.pca %>% select(x=PC1, value=PC2, variable=Species), colors=mycolors[1:3])
plot(grf)


