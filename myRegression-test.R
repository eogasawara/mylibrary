source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myRegression.R")
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/mySample.R")

loadlibrary("MASS")

print(t(sapply(Boston, class)))
head(Boston)
?MASS::Boston

# preparing dataset for random sampling
set.seed(1)
sr <- sample_random(Boston)
sr <- train_test(sr)
boston_train = sr$train
boston_test = sr$test

train_test <- function(model, boston_train, boston_test) {
  print(class(model)[1])
  
  loadlibrary("RSNNS")
  boston_train_predictand = boston_train[,"medv"]
  boston_test_predictand = boston_test[,"medv"]
  
  model <- prepare(model)
  train_prediction <- action(model)
  
  train_eval <- regression_evaluation(boston_train_predictand, train_prediction)
  train_eval <- prepare(train_eval)
  print(action(train_eval))

  model$data <- boston_test
  test_prediction <- action(model)
  
  test_eval <- regression_evaluation(boston_test_predictand, test_prediction)
  test_eval <- prepare(test_eval)
  print(action(test_eval))
}

train_test(regression_decision_tree(boston_train, "medv"), boston_train, boston_test)
train_test(regression_random_forest(boston_train, "medv"), boston_train, boston_test)
train_test(regression_mlp_nnet(boston_train, "medv"), boston_train, boston_test)
train_test(regression_svm(boston_train, "medv"), boston_train, boston_test)
train_test(regression_knn(boston_train, "medv"), boston_train, boston_test)

