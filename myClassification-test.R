source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myClassification.R")

source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/mySample.R")
data(iris)
head(iris)

# preparing dataset for random sampling
set.seed(1)
sr <- sample_random(iris)
sr <- train_test(sr)
iris_train = sr$train
iris_test = sr$test


tbl <- rbind(table(iris$Species), table(iris_train$Species), table(iris_test$Species))
rownames(tbl) <- c("dataset", "training", "test")
head(tbl)


train_test <- function(model, iris_train, iris_test) {
  print(className(model))
  
  loadlibrary("RSNNS")
  iris_train_predictand = decodeClassLabels(iris_train[,"Species"])
  iris_test_predictand = decodeClassLabels(iris_test[,"Species"])
  
  model <- prepare(model)
  train_prediction <- action(model)
  
  train_eval <- evaluation(iris_train_predictand, train_prediction)
  train_eval <- prepare(train_eval)
  print(action(train_eval))
  plot(roc_curve(train_eval))
  
  model$data <- iris_test
  test_prediction <- action(model)
  
  test_eval <- evaluation(iris_test_predictand, test_prediction)
  test_eval <- prepare(test_eval)
  print(action(test_eval))
  plot(roc_curve(test_eval))
}

train_test(classification_zerorule(iris_train, "Species"), iris_train, iris_test)


#head(model$train$metrics)
#test <- ZeroRule_test <- class_test(model, iris_test, "Species")
#head(test$predictions)
