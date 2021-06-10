# version 1.0
#source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myRegression.R")
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/mySample.R")

loadlibrary("MASS")
data(Boston)
print(t(sapply(Boston, class)))
head(Boston)
#dataset catalog
#?MASS::Boston


#Boston <- as.matrix(Boston)

# preparing dataset for random sampling
set.seed(1)
sr <- sample_random()
sr <- train_test(sr, Boston)
boston_train = sr$train
boston_test = sr$test

train_test <- function(model, boston_train, boston_test) {
  set.seed(1)
  print(class(model)[1])
  
  loadlibrary("RSNNS")
  
  model <- prepare(model, boston_train)
  
  train_prediction <- action(model, boston_train)
  boston_train_predictand = boston_train[,"medv"]
  train_eval <- regression_evaluation(boston_train_predictand, train_prediction)
  print(train_eval$metrics)

  test_prediction <- action(model, boston_test)
  boston_test_predictand = boston_test[,"medv"]
  test_eval <- regression_evaluation(boston_test_predictand, test_prediction)
  print(test_eval$metrics)
}

if (FALSE) {
  train_test(reg_dtree("medv"), boston_train, boston_test)
  train_test(reg_rf("medv", mtry=5, ntree=375), boston_train, boston_test)
  train_test(reg_mlp("medv", neurons=5, decay=0.08), boston_train, boston_test)
  train_test(reg_svm("medv", epsilon=0.1, cost=20.000), boston_train, boston_test)
  train_test(reg_knn("medv", k=2), boston_train, boston_test)
  train_test(reg_cnn("medv", epochs = 200), boston_train, boston_test)
}

train_test(reg_cnn("medv", epochs = 200), boston_train, boston_test)

