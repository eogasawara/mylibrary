# version 1.2
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myBasic.R")
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myPreprocessing.R")
#source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myClassification.R")

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
  
  model <- prepare(model, iris_train)
  train_prediction <- action(model, iris_train)
  
  iris_train_predictand = decodeClassLabels(iris_train[,"Species"])
  train_eval <- evaluation.classification(iris_train_predictand, train_prediction)
  print(train_eval$metrics)
  plot(roc_curve(train_eval))
  
  test_prediction <- action(model, iris_test)
  
  iris_test_predictand = decodeClassLabels(iris_test[,"Species"])
  test_eval <- evaluation.classification(iris_test_predictand, test_prediction)
  print(test_eval$metrics)
  plot(roc_curve(test_eval))
}


if (TRUE) {
  train_test(classification_majority("Species", slevels), iris_train, iris_test)
  train_test(classification_dtree("Species", slevels), iris_train, iris_test)
  train_test(classification_nb("Species", slevels), iris_train, iris_test)
  train_test(classification_rf("Species", slevels, mtry=3, ntree=5), iris_train, iris_test)
  train_test(classification_mlp("Species", slevels, size=3,decay=0.03), iris_train, iris_test)
  train_test(classification_svm("Species", slevels, epsilon=0.0,cost=20.000), iris_train, iris_test)
  train_test(classification_knn("Species", slevels, k=1), iris_train, iris_test)
  train_test(classification_cnn("Species", slevels, neurons=16,epochs=150), iris_train, iris_test)
}

