## 21-classification-iris.R
## Normalize Iris features, one-hot encode Species, and split data.

iris <- datasets::iris

norm <- minmax()
norm <- fit(norm, iris)
iris <- transform(norm, iris)

cm <- categ_mapping("Species")
iris_cm <- transform(cm, iris)

iris <- cbind(iris, iris_cm)
iris$Species <- NULL

colnames(iris) <- c("sepal_length", "sepal_width", "petal_length", "petal_width", "setosa", "versicolor", "virginica")

set.seed(1)
sr <- sample_random()
sr <- train_test(sr, iris)
iris_train <- sr$train
iris_test <- sr$test

dir.create("pytorch/data", showWarnings = FALSE, recursive = TRUE)
write.table(iris_train, file="pytorch/data/iris_train.csv", quote=FALSE, sep = ",", row.names = FALSE)
write.table(iris_test, file="pytorch/data/iris_test.csv", quote=FALSE, sep = ",", row.names = FALSE)
