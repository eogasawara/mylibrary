source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myPreprocessing.R")
data(iris)
table(iris$Species)

# samples
mysample <- sample.random(iris)
table(mysample$sample$Species)

mysample <- sample.stratified(iris, "Species")
table(mysample$sample$Species)

myfolds <- sample.random_kfold(iris, k=4)
for (i in (1:length(myfolds)))
  print(table(myfolds[[i]]$Species))

myfolds <- sample.stratified_kfold(iris, "Species", k=4)
for (i in (1:length(myfolds)))
  print(table(myfolds[[i]]$Species))


# outlier analysis
out <- outliers.boxplot(iris)
myiris <- iris[!out,]
head(iris[out,])

myirism <- normalize.minmax(iris)
hist(myirism$data$Sepal.Width)

myirisz <- normalize.zscore(iris, nmean = 0.5,nsd = 0.5/2.698)
hist(myirisz$data$Sepal.Width)

# DATA TRANSFORMATION: PCA
mypca <- dt.pca(iris, "Species")
head(mypca$pca)
head(mypca$pca.transf)

# DATA TRANSFORMATION: DISCRETIZATION
Sepal.Bin.f <- binning.freq(iris$Sepal.Length, n=2)
print(Sepal.Bin.f$interval)
Sepal.Bin.i <- binning.interval(iris$Sepal.Length, n=2)
print(Sepal.Bin.i$interval)
Sepal.Bin.c <- binning.cluster(iris$Sepal.Length, n=2)
print(Sepal.Bin.c$interval)
Sepal.Bin.fo <- binning.opt(iris$Sepal.Length, binning=binning.freq)
print(Sepal.Bin.fo$interval)
Sepal.Bin.io <- binning.opt(iris$Sepal.Length, binning=binning.interval)
print(Sepal.Bin.io$interval)
Sepal.Bin.co <- binning.opt(iris$Sepal.Length, binning=binning.cluster)
print(Sepal.Bin.co$interval)

# DATA BALANCING
myiris <- iris[c(1:20,51:100, 110:120),]
print(table(myiris$Species))
myiris.bo <- balance.oversampling(myiris, "Species")
print(table(myiris.bo$Species))
myiris.bs <- balance.subsampling(myiris, "Species")
print(table(myiris.bs$Species))

head(iris)
mycm <- dt.categ_mapping(iris, "Species")
head(mycm)

