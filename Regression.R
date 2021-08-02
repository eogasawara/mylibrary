# version 1.2
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myBasic.R")
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myPreprocessing.R")
#source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myRegression.R")

loadlibrary("MASS")
data(Boston)
print(t(sapply(Boston, class)))
head(Boston)

# for performance issues, you can use matrix
Boston <- as.matrix(Boston)

# preparing dataset for random sampling
set.seed(1)
sr <- sample_random()
sr <- train_test(sr, Boston)
boston_train = sr$train
boston_test = sr$test

train_test <- function(model, boston_train, boston_test) {
  print(class(model)[1])

  model <- train(model, boston_train)
  
  train_prediction <- predict(model, boston_train)
  boston_train_predictand = boston_train[,"medv"]
  train_eval <- evaluation.regression(boston_train_predictand, train_prediction)
  print(train_eval$metrics)

  test_prediction <- predict(model, boston_test)
  boston_test_predictand = boston_test[,"medv"]
  test_eval <- evaluation.regression(boston_test_predictand, test_prediction)
  print(test_eval$metrics)
}

train_test(regression_dtree("medv"), boston_train, boston_test)

# do not set mtry and ntree for hyperparameter optimization
# you can also set a range for them
train_test(regression_rf("medv", mtry=7,ntree=30), boston_train, boston_test)

# do not set neurons and decay for hyperparameter optimization
# you can also set a range for them
train_test(regression_mlp("medv", size=5,decay=0.54), boston_train, boston_test)

#do not set epsilon, cost, and  kernel for hyperparameter optimization
# you can also set a range for them
train_test(regression_svm("medv", epsilon=0.2,cost=40.000), boston_train, boston_test)

# do not set k for hyperparameter optimization
# you can also set a range for it
train_test(regression_knn("medv", k=5), boston_train, boston_test)

# do not set neurons and epochs for hyperparameter optimization
# you can also set a range for them  
train_test(regression_cnn("medv", neurons=32,epochs=200), boston_train, boston_test)


