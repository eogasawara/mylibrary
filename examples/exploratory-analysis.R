source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myGraphics.R")
loadlibrary("MASS")
loadlibrary("RColorBrewer")
loadlibrary("gridExtra")
loadlibrary("GGally")

col.set <- brewer.pal(11, 'Spectral')
mycolors <- col.set[c(1,3,5,7,9)]


data(iris)
summary(iris$Sepal.Length)
table(iris$Sepal.Length)
sepal.length = round(iris$Sepal.Length)
table(sepal.length)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmode(round(iris$Sepal.Length))

series <- data.frame(variable="Sepal", value=iris$Sepal.Length)
grfA <- plot.density(series, colors=mycolors[3])

series <- data.frame(variable=iris$Species, value=iris$Sepal.Length)
grfB <- plot.density(series, colors=mycolors[c(1:3)])

options(repr.plot.width=7, repr.plot.height=3)
grid.arrange(grfA, grfB, ncol=2)


series <- data.frame(variable="Sepal", value=iris$Sepal.Length)
grfA <- plot.boxplot(series, colors=mycolors[3])

series <- data.frame(variable=iris$Species, value=iris$Sepal.Length)
grfB <- plot.boxplot(series, colors=mycolors[3])

options(repr.plot.width=7, repr.plot.height=3)
grid.arrange(grfA, grfB, ncol=2)


series <- data.frame(variable="Sepal", value=iris$Sepal.Length)
grfA <- plot.hist(series, colors=mycolors[3], bin=0.5)

series <- data.frame(variable=iris$Species, value=iris$Sepal.Length)
grfB <- plot.hist(series, colors=mycolors[c(1:3)], bin=0.5)

options(repr.plot.width=7, repr.plot.height=3)
grid.arrange(grfA, grfB, ncol=2)

options(repr.plot.width=4, repr.plot.height=3)
qqnorm(iris$Sepal.Length)
qqline(iris$Sepal.Length)

options(repr.plot.width=4, repr.plot.height=3)
qqplot(iris$Petal.Length, iris$Sepal.Length)
fm <- lm(iris$Sepal.Length ~ iris$Petal.Length)
abline(coef(fm), lty=4)

ir <- iris
ir$Species <- NULL
parcoord(log(ir)[, c(3, 4, 2, 1)], col = 1 + (0:149)%/%50)

pairs(~.,data=ir, main="Scatterplot Matrix")


ggpairs(iris[,], aes(colour = Species, alpha = 0.4))

