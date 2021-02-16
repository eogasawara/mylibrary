#https://tensorflow.rstudio.com/tutorials/beginners/
#https://www.r-bloggers.com/2018/11/lstm-with-keras-tensorflow/

library(keras)
to_categorical(0:3)

rm(list=ls())
data(iris)
plot(iris$Petal.Length,
     iris$Petal.Width, col = iris$Species)

onehot.species = to_categorical(as.numeric(iris$Species) - 1)
iris = as.matrix(iris[, 1:4])
iris = cbind(iris, onehot.species)

set.seed(17)
ind <- sample(2, nrow(iris),
              replace = TRUE, prob = c(0.7, 0.3))
iris.training <- iris[ind == 1, 1:4]
iris.test <- iris[ind == 2, 1:4]
iris.trainingtarget <- iris[ind == 1, -seq(4)]
iris.testtarget <- iris[ind == 2, -(1:4)]

model <- keras_model_sequential()

model %>%
  layer_dense(units = ncol(iris.trainingtarget), activation = 'softmax',
              input_shape = ncol(iris.training))
summary(model)

sgd <- optimizer_sgd(lr = 0.01)

model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = sgd,
  metrics = 'accuracy'
)

history <- model %>% fit(
  x = iris.training,
  y = iris.trainingtarget,
  epochs = 500,
  batch_size = 5,
  validation_split = 0.2,
  verbose = 0
)

plot(history)

classes <- model %>% predict_classes(iris.test)
table(iris.testtarget%*%0:2, classes)

(score <- model %>% evaluate(iris.test, iris.testtarget))
