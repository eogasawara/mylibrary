# version 0.9
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myGraphic.R")

loadlibrary("MASS")
data(iris)

head(iris[c(1:3,51:53,101:103),])

sum <- summary(iris$Sepal.Length)
sum

IQR <- sum["3rd Qu."]-sum["1st Qu."]
print(sprintf("IQR=%.1f", IQR))

plot_size(4, 3)

hist(iris$Sepal.Length)

op <- par(mfrow=c(2,2))
hist(iris$Sepal.Length)
hist(iris$Sepal.Width)
hist(iris$Sepal.Length)
hist(iris$Sepal.Width)

par(op)

loadlibrary("reshape")
data <- melt(iris)
head(data)

loadlibrary("RColorBrewer")
col.set <- brewer.pal(11, 'Spectral')
mycolors <- col.set[c(1,3,5,7,9)]

grfA <- plot.boxplot(data, colors="white")
plot(grfA)

loadlibrary("gridExtra")
loadlibrary("dplyr")
grfA <- plot.density(iris %>% dplyr::select(variable=Sepal.Length, value=Sepal.Length), label_x = "Sepal.Length", color="white")
grfB <- plot.density(iris %>% dplyr::select(variable=Sepal.Width, value=Sepal.Width), label_x = "Sepal.Width", color="white")
grfC <- plot.density(iris %>% dplyr::select(variable=Petal.Length, value=Petal.Length), label_x = "Petal.Length", color="white")
grfD <- plot.density(iris %>% dplyr::select(variable=Petal.Width, value=Petal.Width), label_x = "Petal.Width", color="white")
plot_size(8, 6)
grid.arrange(grfA, grfB, grfC, grfD, ncol=2, nrow=2)
plot_size(4, 3)

grfA <- plot.density(iris %>% select(variable=Species, value=Sepal.Length), label_x = "Sepal.Length", color=mycolors[c(1:3)])
grfB <- plot.density(iris %>% select(variable=Species, value=Sepal.Width), label_x = "Sepal.Width", color=mycolors[c(1:3)])
grfC <- plot.density(iris %>% select(variable=Species, value=Petal.Length), label_x = "Petal.Length", color=mycolors[c(1:3)])
grfD <- plot.density(iris %>% select(variable=Species, value=Petal.Width), label_x = "Petal.Width", color=mycolors[c(1:3)])
plot_size(8, 6)
grid.arrange(grfA, grfB, grfC, grfD, ncol=2, nrow=2)
plot_size(4, 3)

grfA <- plot.boxplot(iris %>% select(variable=Species, value=Sepal.Length), label_x = "Sepal.Length", color=mycolors[c(1:3)])
grfB <- plot.boxplot(iris %>% select(variable=Species, value=Sepal.Width), label_x = "Sepal.Width", color=mycolors[c(1:3)])
grfC <- plot.boxplot(iris %>% select(variable=Species, value=Petal.Length), label_x = "Petal.Length", color=mycolors[c(1:3)])
grfD <- plot.boxplot(iris %>% select(variable=Species, value=Petal.Width), label_x = "Petal.Width", color=mycolors[c(1:3)])
plot_size(8, 6)
grid.arrange(grfA, grfB, grfC, grfD, ncol=2, nrow=2)
plot_size(4, 3)


plot(iris$Sepal.Length,iris$Sepal.Width)

grf <- exp_correlation(iris[,1:4], color = mycolors[1:3]) 

plot(grf)

loadlibrary("WVPlots")
grf <- exp_pair_plot(data=iris, cnames=colnames(iris)[1:4], title="Iris", colors=mycolors[1])

plot_size(7, 5)
grf
plot_size(4, 3)

grf <- exp_pair_plot(data=iris, cnames=colnames(iris)[1:4], clabel='Species', title="Iris", colors=mycolors[1:3])

plot_size(8, 5)
grf
plot_size(4, 3)

loadlibrary("GGally")
grf <- exp_advpair_plot(data=iris, cnames=colnames(iris)[1:4], title="Iris", colors=mycolors[1])

plot_size(7, 5)
grf
plot_size(4, 3)

grf <- exp_advpair_plot(data=iris, cnames=colnames(iris)[1:4], title="Iris", clabel='Species', colors=mycolors[1:3])

plot_size(8, 5)
grf
plot_size(4, 3)

grf <- ggparcoord(data = iris, columns = c(1:4), group=5) + theme_bw(base_size = 10) + scale_color_manual(values=mycolors[1:3])

plot_size(5, 3)
plot(grf)
plot_size(4, 3)


mat <- as.matrix(iris[,1:4])
x <- (1:nrow(mat))
y <- (1:ncol(mat))
image(x, y, mat, col = col.set, axes = FALSE,  main = "Iris", xlab="sample", ylab="Attributes")
axis(2, at = seq(0, ncol(mat), by = 1))
axis(1, at = seq(0, nrow(mat), by = 10))

set.seed(1)
sample_rows = sample(1:nrow(iris), 25)

isample = iris[sample_rows,]
labels = as.character(rownames(isample))
isample$Species <- NULL


loadlibrary("aplpack")
plot_size(8, 6)
faces(isample, labels = labels, print.info=F, cex=1)
plot_size(4, 3)

set.seed(1)
sample_rows = sample(1:nrow(iris), 25)

isample = iris[sample_rows,]
labels = as.character(isample$Species)
isample$Species <- NULL

plot_size(8, 6)
faces(isample, labels = labels, print.info=F, cex=1)
plot_size(4, 3)
