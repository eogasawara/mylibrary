# version 1.0
#source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myClassification.R")
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/mySample.R")

data(iris)
head(iris)

slevels <- levels(iris$Species)

iris <- cbind(as.matrix(iris[,1:4]), Species=iris$Species)

# preparing dataset for random sampling
set.seed(1)
sr <- sample_random()
sr <- train_test(sr, iris)
iris_train = sr$train
iris_test = sr$test
tbl <- rbind(table(iris[,"Species"]), table(iris_train[,"Species"]), table(iris_test[,"Species"]))
rownames(tbl) <- c("dataset", "training", "test")
head(tbl)


train_test <- function(model, iris_train, iris_test) {
  set.seed(1)
  print(class(model)[1])
  
  loadlibrary("RSNNS")
  
  model <- prepare(model, iris_train)
  train_prediction <- action(model, iris_train)
  
  iris_train_predictand = decodeClassLabels(iris_train[,"Species"])
  train_eval <- classif_evaluation(iris_train_predictand, train_prediction)
  print(train_eval$metrics)
  plot(roc_curve(train_eval))
  
  test_prediction <- action(model, iris_test)
  
  iris_test_predictand = decodeClassLabels(iris_test[,"Species"])
  test_eval <- classif_evaluation(iris_test_predictand, test_prediction)
  print(test_eval$metrics)
  plot(roc_curve(test_eval))
}


if (FALSE) {
  train_test(class_majority("Species", slevels), iris_train, iris_test)
  train_test(class_dtree("Species", slevels), iris_train, iris_test)
  train_test(class_nb("Species", slevels), iris_train, iris_test)
  train_test(class_rf("Species", slevels, mtry=2, ntree=50), iris_train, iris_test)
  train_test(class_mlp("Species", slevels, neurons=2,decay=0.04), iris_train, iris_test)
  train_test(class_svm("Species", slevels, epsilon=0.0,cost=5.000), iris_train, iris_test)
  train_test(class_knn("Species", slevels), iris_train, iris_test)
  train_test(class_cnn("Species", slevels, epochs = 200), iris_train, iris_test)
}

train_test(class_cnn("Species", slevels, epochs = 200), iris_train, iris_test)
