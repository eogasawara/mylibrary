source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myGraphic.R")

plot.size(12, 5)
colors <- brewer.pal(11, 'Paired')
font <- theme(text = element_text(size=16))

loadlibrary("MASS")
data(iris)

head(iris[c(1:3,51:53,101:103),])

sum <- summary(iris$Sepal.Length)
sum

IQR <- sum["3rd Qu."]-sum["1st Qu."]
print(sprintf("IQR=%.1f", IQR))

plot.hist(iris %>% dplyr::select(Sepal.Length), 
          label_x = "Sepal Length", color=colors[1]) + font

grf1 <- plot.hist(iris %>% dplyr::select(Sepal.Length), 
                  label_x = "Sepal.Length", color=colors[1]) + font
grf2 <- plot.hist(iris %>% dplyr::select(Sepal.Width), 
                  label_x = "Sepal.Width", color=colors[1]) + font  
grf3 <- plot.hist(iris %>% dplyr::select(Sepal.Length), 
                  label_x = "Sepal.Length", color=colors[1]) + font 
grf4 <- plot.hist(iris %>% dplyr::select(Sepal.Width), 
                  label_x = "Sepal.Width", color=colors[1]) + font

loadlibrary("gridExtra") 
grid.arrange(grf1, grf2, grf3, grf4, ncol=2)


loadlibrary("reshape")
data <- melt(iris)
head(data)

grfA <- plot.boxplot(iris, colors=colors[1])
plot(grfA)

grf1 <- plot.density(iris %>% dplyr::select(Sepal.Length), 
                  label_x = "Sepal.Length", color=colors[1]) + font
grf2 <- plot.density(iris %>% dplyr::select(Sepal.Width), 
                  label_x = "Sepal.Width", color=colors[1]) + font  
grf3 <- plot.density(iris %>% dplyr::select(Sepal.Length), 
                  label_x = "Sepal.Length", color=colors[1]) + font 
grf4 <- plot.density(iris %>% dplyr::select(Sepal.Width), 
                  label_x = "Sepal.Width", color=colors[1]) + font

grid.arrange(grf1, grf2, grf3, grf4, ncol=2)


grfA <- plot.density.class(iris %>% dplyr::select(Species, Sepal.Length), 
                           class_label="Species", label_x = "Sepal.Length", color=colors[c(1:3)])
grfB <- plot.density.class(iris %>% dplyr::select(Species, Sepal.Width), 
                           class_label="Species", label_x = "Sepal.Width", color=colors[c(1:3)])
grfC <- plot.density.class(iris %>% dplyr::select(Species, Petal.Length), 
                           class_label="Species", label_x = "Petal.Length", color=colors[c(1:3)])
grfD <- plot.density.class(iris %>% dplyr::select(Species, Petal.Width), 
                           class_label="Species", label_x = "Petal.Width", color=colors[c(1:3)])
grid.arrange(grfA, grfB, grfC, grfD, ncol=2, nrow=2)


grfA <- plot.boxplot.class(iris %>% dplyr::select(Species, Sepal.Length), 
                           class_label="Species", label_x = "Sepal.Length", color=colors[c(1:3)])
grfB <- plot.boxplot.class(iris %>% dplyr::select(Species, Sepal.Width), 
                           class_label="Species", label_x = "Sepal.Width", color=colors[c(1:3)])
grfC <- plot.boxplot.class(iris %>% dplyr::select(Species, Petal.Length), 
                           class_label="Species", label_x = "Petal.Length", color=colors[c(1:3)])
grfD <- plot.boxplot.class(iris %>% dplyr::select(Species, Petal.Width), 
                           class_label="Species", label_x = "Petal.Width", color=colors[c(1:3)])
grid.arrange(grfA, grfB, grfC, grfD, ncol=2, nrow=2)

grf <- plot.scatter(iris %>% dplyr::select(Sepal.Length, Sepal.Width), 
                    label_x = "Sepal.Length", 
                    label_y = "Sepal.Width", colors=colors[1]) +
                    theme(legend.position = "none") 
plot(grf)

grf <- plot.scatter.class(iris %>% dplyr::select(Sepal.Length, Sepal.Width, Species), 
                    class_label="Species",
                    label_x = "Sepal.Length", label_y = "Sepal.Width", colors=colors[1:3])
plot(grf)

plot.correlation(iris %>% 
                 dplyr::select(Sepal.Width, Sepal.Length, Petal.Width, Petal.Length))

grf <- plot.pair(data=iris, cnames=colnames(iris)[1:4], 
                 title="Iris", colors=colors[1])
grf


grf <- plot.pair(data=iris, cnames=colnames(iris)[1:4], 
                 clabel='Species', title="Iris", colors=colors[1:3])
grf


grf <- plot.pair.adv(data=iris, cnames=colnames(iris)[1:4], 
                     title="Iris", colors=colors[1])
grf

grf <- plot.pair.adv(data=iris, cnames=colnames(iris)[1:4], 
                        title="Iris", clabel='Species', colors=colors[1:3])
grf


grf <- ggparcoord(data = iris, columns = c(1:4), group=5) + 
    theme_bw(base_size = 10) + scale_color_manual(values=colors[1:3])
plot(grf)

mat <- as.matrix(iris[,1:4])
x <- (1:nrow(mat))
y <- (1:ncol(mat))
image(x, y, mat, col = colors, axes = FALSE,  
      main = "Iris", xlab="sample", ylab="Attributes")
axis(2, at = seq(0, ncol(mat), by = 1))
axis(1, at = seq(0, nrow(mat), by = 10))

set.seed(1)
sample_rows = sample(1:nrow(iris), 25)

isample = iris[sample_rows,]
labels = as.character(rownames(isample))
isample$Species <- NULL

loadlibrary("aplpack")
faces(isample, labels = labels, print.info=F, cex=1)

set.seed(1)
sample_rows = sample(1:nrow(iris), 25)

isample = iris[sample_rows,]
labels = as.character(isample$Species)
isample$Species <- NULL

faces(isample, labels = labels, print.info=F, cex=1)


