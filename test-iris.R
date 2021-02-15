source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myTransform.R")

#https://tensorflow.rstudio.com/tutorials/beginners/
#https://www.r-bloggers.com/2018/11/lstm-with-keras-tensorflow/

loadlibrary("dplyr")
loadlibrary("tfdatasets")
loadlibrary("tensorflow")
loadlibrary("keras")  

print_dot_callback <- callback_lambda(
  on_epoch_end = function(epoch, logs) {
    if (epoch %% 800 == 0) cat("\n")
    if (epoch %% 10 == 0) cat(".")
  }
)  

data <- iris

obj <- dal_transform()
obj$attribute <- "Species"

onehot.species = to_categorical(as.numeric(data[,obj$attribute]) - 1)
obj$predictors <- setdiff(colnames(data), obj$attribute) 
data[,obj$attribute] <- NULL
data = cbind(data, onehot.species)

set.seed(17)
ind <- sample(2, nrow(iris),
              replace = TRUE, prob = c(0.7, 0.3))
data.training <- as.matrix(data[ind == 1, obj$predictors])
data.test <- as.matrix(data[ind == 2, obj$predictors])
target.training <- as.matrix(onehot.species[ind == 1, ])
target.test <- as.matrix(onehot.species[ind == 2, ])

model <- keras_model_sequential()

model %>%
  layer_dense(units = ncol(target.training), activation = 'softmax',
              input_shape = ncol(data.training))
summary(model)

sgd <- optimizer_sgd(lr = 0.01)

model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = sgd,
  metrics = 'accuracy'
)

history <- model %>% fit(
  x = data.training,
  y = target.training,
  epochs = 500,
  batch_size = 5,
  validation_split = 0.2,
  verbose = 0,
  callbacks = list(print_dot_callback)
)

plot(history)

classes <- model %>% predict_classes(data.test)
table(target.test%*%0:2, classes)

(score <- model %>% evaluate(data.test, target.test))
