#https://tensorflow.rstudio.com/tutorials/beginners/
#https://www.r-bloggers.com/2018/11/lstm-with-keras-tensorflow/
#install.packages("tensorflow")
#install.packages("keras")
#install.packages("tfdatasets")
#library(tensorflow)
#install_tensorflow()

startup <- function() {
  library(tensorflow)
  library(keras)
  tf$constant("Hellow Tensorflow")

  library(tfdatasets)
  
  mnist <- dataset_mnist()
  mnist$train$x <- mnist$train$x/255
  mnist$test$x <- mnist$test$x/255
  
  model <- keras_model_sequential() %>% 
    layer_flatten(input_shape = c(28, 28)) %>% 
    layer_dense(units = 128, activation = "relu") %>% 
    layer_dropout(0.2) %>% 
    layer_dense(10, activation = "softmax")
  
  summary(model)
  
  model %>% 
    compile(
      loss = "sparse_categorical_crossentropy",
      optimizer = "adam",
      metrics = "accuracy"
    )
  
  model %>% 
    fit(
      x = mnist$train$x, y = mnist$train$y,
      epochs = 5,
      validation_split = 0.3,
      verbose = 2
    )
  
  predictions <- predict(model, mnist$test$x)
  head(predictions, 2)
  
  model %>% 
    evaluate(mnist$test$x, mnist$test$y, verbose = 0)
  
  
  model_dir <- "C:/Users/eduar/OneDrive/Git/research/events/models"

  save_model_tf(object = model, filepath = sprintf("%s/%s", model_dir, "model"))

  reloaded_model <- load_model_tf(sprintf("%s/%s", model_dir, "model"))
  
  all.equal(predict(model, mnist$test$x), predict(reloaded_model, mnist$test$x))  
}

library(keras)
library(tfdatasets)
library(dplyr)
library(ggplot2)

boston_housing <- dataset_boston_housing()

c(train_data, train_labels) %<-% boston_housing$train
c(test_data, test_labels) %<-% boston_housing$test

paste0("Training entries: ", length(train_data), ", labels: ", length(train_labels))

train_data[1, ] # Display sample features, notice the different scales


column_names <- c('CRIM', 'ZN', 'INDUS', 'CHAS', 'NOX', 'RM', 'AGE', 
                  'DIS', 'RAD', 'TAX', 'PTRATIO', 'B', 'LSTAT')

train_df <- train_data %>% 
  as_tibble(.name_repair = "minimal") %>% 
  setNames(column_names) %>% 
  mutate(label = train_labels)

test_df <- test_data %>% 
  as_tibble(.name_repair = "minimal") %>% 
  setNames(column_names) %>% 
  mutate(label = test_labels)

train_labels[1:10]



build_model <- function(train_df) {
  set.seed(1)
  
  spec <- feature_spec(train_df, label ~ . ) %>% 
    step_numeric_column(all_numeric(), normalizer_fn = scaler_standard()) %>% 
    fit()

  input <- layer_input_from_dataset(train_df %>% select(-label))
  
  output <- input %>% 
    layer_dense_features(dense_features(spec)) %>% 
    layer_dense(units = 64, activation = "relu") %>%
    layer_dense(units = 64, activation = "relu") %>%
    layer_dense(units = 1) 
  
  model <- keras_model(input, output)
  
  model %>% 
    compile(
      loss = "mse",
      optimizer = optimizer_rmsprop(),
      metrics = list("mean_absolute_error")
    )

  return(model)
}

model <- build_model(train_df)

print_dot_callback <- callback_lambda(
  on_epoch_end = function(epoch, logs) {
    if (epoch %% 80 == 0) cat("\n")
    cat(".")
  }
)    

history <- model %>% fit(
  x = train_df %>% select(-label),
  y = train_df$label,
  epochs = 500,
  validation_split = 0.2,
  verbose = 0,
  callbacks = list(print_dot_callback)
)

plot(history)
c(loss, mae) %<-% (model %>% evaluate(test_df %>% select(-label), test_df$label, verbose = 0))
paste0("Mean absolute error on test set: $", sprintf("%.2f", mae * 1000))

test_predictions <- model %>% predict(test_df %>% select(-label))
test_predictions[ , 1]