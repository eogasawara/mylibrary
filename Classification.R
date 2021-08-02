# version 1.2
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myBasic.R")
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myPreprocessing.R")
#source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myClassification.R")

iris <- datasets::iris
head(iris)

#extracting the levels for the dataset
slevels <- levels(iris$Species)
slevels

#for performance issues, you can use matrix instead of data.frame
iris <- cbind(as.matrix(iris[,1:4]), Species=iris$Species)

# preparing dataset for random sampling
set.seed(1)
sr <- sample_random()
sr <- train_test(sr, iris)
iris_train = sr$train
iris_test = sr$test

tbl <- rbind(table(iris[,"Species"]), 
             table(iris_train[,"Species"]), 
             table(iris_test[,"Species"]))
rownames(tbl) <- c("dataset", "training", "test")
head(tbl)

train_test <- function(model, iris_train, iris_test) {
  print(class(model)[1])
  
  model <- train(model, iris_train)
  train_prediction <- predict(model, iris_train)
  
  iris_train_predictand = decodeClassLabels(iris_train[,"Species"])
  train_eval <- evaluation.classification(iris_train_predictand, train_prediction)
  print(train_eval$metrics)
  plot(roc_curve(train_eval))
  
  test_prediction <- predict(model, iris_test)
  
  iris_test_predictand = decodeClassLabels(iris_test[,"Species"])
  test_eval <- evaluation.classification(iris_test_predictand, test_prediction)
  print(test_eval$metrics)
  plot(roc_curve(test_eval))
}


train_test(classification_majority("Species", slevels), iris_train, iris_test)

train_test(classification_dtree("Species", slevels), iris_train, iris_test)

train_test(classification_nb("Species", slevels), 
           iris_train, iris_test)

# do not set mtry and ntree for hyperparameter optimization
# you can also set a range for them
train_test(classification_rf("Species", slevels, mtry=3, ntree=5), 
           iris_train, iris_test)

# do not set decay and set a range for neurons for hyperparameter optimization
# you can also set a range for them
  train_test(classification_mlp("Species", slevels, size=3,decay=0.03),
             iris_train, iris_test)

#do not set epsilon, cost, and  kernel for hyperparameter optimization
# you can also set a range for them
train_test(classification_svm("Species", slevels, epsilon=0.0,cost=20.000), 
           iris_train, iris_test)

# do not set k for hyperparameter optimization
# you can also set a range for it
train_test(classification_knn("Species", slevels, k=1), iris_train, iris_test)

# do not set neurons and epochs for hyperparameter optimization
# you can also set a range for them
train_test(classification_cnn("Species", slevels, neurons=16,epochs=150), 
           iris_train, iris_test)


