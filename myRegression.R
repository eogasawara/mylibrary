# version 1.0
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myTransform.R")
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/mySample.R")


#loadlibrary("kernlab")
#loadlibrary("rattle")
#loadlibrary("MASS")

# regression
regression <- function(attribute) {
  obj <- dal_transform()
  obj$attribute <- attribute

  class(obj) <- append("regression", class(obj))    
  return(obj)
}

prepare.regression <- function(obj, data) {
  obj <- start_log(obj) 
  obj$predictors <- setdiff(colnames(data), obj$attribute)  
  return(obj)
}

# decision_tree
reg_dtree <- function(attribute) {
  obj <- regression(attribute)

  class(obj) <- append("reg_dtree", class(obj))    
  return(obj)
}

prepare.reg_dtree <- function(obj, data) {
  data <- adjust.data.frame(data)
  obj <- prepare.regression(obj, data)  
  loadlibrary("tree")
  
  regression <- formula(paste(obj$attribute, "  ~ ."))  
  obj$model <- tree(regression, data)
  
  obj <- register_log(obj)
  return(obj)
}

action.reg_dtree <- function(obj, data) {
  data <- adjust.data.frame(data)
  predictors <- data[,obj$predictors]   
  prediction <- predict(obj$model, predictors, type="vector")  
  return(prediction)
}

# random_forest
reg_rf <- function(attribute, mtry = NULL, ntree = seq(5, 50, 5)) {
  obj <- regression(attribute)
  
  if (is.null(mtry))
    mtry <- unique(1:ceiling(ncol(data)/3))
  obj$mtry <- mtry
  obj$ntree <- ntree

  class(obj) <- append("reg_rf", class(obj))    
  return(obj)
}

prepare.reg_rf <- function(obj, data) {
  data <- adjust.data.frame(data)
  obj <- prepare.regression(obj, data)  

  loadlibrary("e1071")
  loadlibrary("randomForest")
  
  regression <- formula(paste(obj$attribute, "  ~ ."))  
  tuned <- tune.randomForest(regression, data=data, mtry=obj$mtry, ntree=obj$ntree)
  obj$model <- tuned$best.model 
  
  msg <- sprintf("mtry=%d,ntree=%d", obj$model$mtry, obj$model$ntree)
  obj <- register_log(obj, msg)
  return(obj)
}

action.reg_rf  <- function(obj, data) {
  data <- adjust.data.frame(data)
  predictors <- data[,obj$predictors]   
  prediction <- predict(obj$model, predictors)  
  return(prediction)
}

# mlp_nnet
reg_mlp <- function(attribute, neurons=NULL, decay=seq(0, 1, 0.1), maxit=1000) {
  obj <- regression(attribute)
  obj$maxit <- maxit
  if (is.null(neurons))
    neurons <- ceiling(ncol(data)/3)
  obj$neurons <- neurons
  obj$decay <- decay
  
  class(obj) <- append("reg_mlp", class(obj))    
  return(obj)
}

prepare.reg_mlp <- function(obj, data) {
  data <- adjust.data.frame(data)
  obj <- prepare.regression(obj, data)  
  
  loadlibrary("e1071")
  loadlibrary("nnet")
  
  regression <- formula(paste(obj$attribute, "  ~ ."))  
  tuned <- tune.nnet(regression, data=data, trace=FALSE, maxit=obj$maxit, decay = obj$decay, size=obj$neurons, linout=TRUE)
  obj$model <- tuned$best.model  
  
  msg <- sprintf("neurons=%d,decay=%.2f", tuned$best.parameters$size, tuned$best.parameters$decay)
  obj <- register_log(obj, msg)
  return(obj)
}

action.reg_mlp  <- function(obj, data) {
  data <- adjust.data.frame(data)
  predictors <- data[,obj$predictors]   
  prediction <- predict(obj$model, predictors)  
  return(prediction)
}

# reg_svm 
reg_svm <- function(attribute, epsilon=seq(0.5,1,0.5), cost=seq(20,100,20), kernel="radial") {
  #kernel: linear, radial, polynomial, sigmoid
  #analisar: https://rpubs.com/Kushan/296706  
  obj <- regression(attribute)
  obj$kernel <- kernel
  obj$epsilon <- epsilon
  obj$cost <- cost
  
  class(obj) <- append("reg_svm", class(obj))    
  return(obj)
}

prepare.reg_svm <- function(obj, data) {
  data <- adjust.data.frame(data)
  obj <- prepare.regression(obj, data)  
  
  loadlibrary("e1071")
  regression <- formula(paste(obj$attribute, "  ~ ."))  
  tuned <- tune.svm(regression, data=data, epsilon=obj$epsilon, cost=obj$cost, kernel="radial")
  obj$model <- tuned$best.model  
  
  msg <- sprintf("epsilon=%.1f,cost=%.3f", obj$model$epsilon, obj$model$cost)
  obj <- register_log(obj, msg)
  return(obj)
}

action.reg_svm  <- function(obj, data) {
  data <- adjust.data.frame(data)
  predictors <- data[,obj$predictors]   
  prediction <- predict(obj$model, predictors) 
  return(prediction)
}

# reg_knn 
reg_knn <- function(attribute, k=1:10) {
  obj <- regression(attribute)
  obj$k <- k
  
  class(obj) <- append("reg_knn", class(obj))    
  return(obj)
}

prepare.reg_knn <- function(obj, data) {
  data <- adjust.data.frame(data)
  obj <- prepare.regression(obj, data)  
  loadlibrary("e1071")
  loadlibrary("FNN")
  
  predictors <- as.matrix(data[,obj$predictors])
  predictand <- data[,obj$attribute]
  
  tuned <- tune.knnreg(train.reg_knn, x = predictors, y = predictand, k = obj$k)
  obj$model <- tuned$best.model
  return(obj)
}

action.reg_knn  <- function(obj, data) {
  #develop from FNN https://daviddalpiaz.github.io/r4sl/knn-reg.html
  data <- adjust.data.frame(data)
  predictors <- as.matrix(data[,obj$predictors])
  prediction <- predict(obj$model, predictors)
  return(prediction)
}

#functions created from tune

train.reg_knn <- function(x, y, k, ...) {
  obj <- list(x=x, y=y, k=k)
  class(obj) <- append("reg_knn_pred", class(obj))    
  return(obj)
}

predict.reg_knn_pred <- function(model, predictors, ...) {
  params <- list(...)
  prediction <- knn.reg(train = model$x, test = predictors, y = model$y, k = model$k)  
  return(prediction$pred)
}

tune.knnreg <- function (x, y = NULL, k=NULL, ...) 
{
  ranges <- list(k = k)
  ranges[vapply(ranges, is.null, NA)] <- NULL
  if (length(ranges) < 1) 
    ranges = NULL
  tuned <- tune("train.reg_knn", train.x = x, train.y = y, ranges = ranges, ...)
  return(tuned)
}

# reg_cnn 

reg_cnn <- function(attribute, neurons=c(2,3,4,5,8,10,16,32,64,128), epochs = 100) {
  obj <- regression(attribute)
  obj$neurons <- neurons
  obj$epochs <- epochs
  
  class(obj) <- append("reg_cnn", class(obj))    
  return(obj)
}

prepare.reg_cnn <- function(obj, data) {
  data <- adjust.data.frame(data)
  obj <- prepare.regression(obj, data)  
  
  loadlibrary("e1071")
  loadlibrary("dplyr")
  loadlibrary("tfdatasets")
  loadlibrary("tensorflow")
  loadlibrary("keras")  
  
  predictors <- data[obj$predictors]
  predictand <- data[,obj$attribute]

  obj$model <- tune.reg_cnn(x = predictors, y = predictand, neurons=obj$neurons, epochs=obj$epochs)
    
  obj <- register_log(obj)
  return(obj)
}

action.reg_cnn  <- function(obj, data) {
  data <- adjust.data.frame(data)
  predictors <- data[,obj$predictors]   
  prediction <- predict(obj$model, predictors)
  return(prediction)
}

#functions created from tune

train.reg_cnn <- function(x, y, neurons, epochs, ...) {
  data <- adjust.data.frame(x)
  data$y <- y
  
  spec <- feature_spec(data, y ~ . ) %>% 
    step_numeric_column(all_numeric(), normalizer_fn = scaler_standard()) %>% 
    fit()
  
  input <- layer_input_from_dataset(data %>% dplyr::select(-y))
  
  output <- input %>% 
    layer_dense_features(dense_features(spec)) %>% 
    layer_dense(units = neurons, activation = "relu") %>% 
    layer_dense(units = neurons, activation = "relu") %>%
    layer_dense(units = 1) 
  
  model <- keras_model(input, output)
  
  model %>% 
    compile(loss = "mse", optimizer = optimizer_rmsprop(), 
            metrics = list("mean_absolute_error"))
  #summary(model)
  
  
  history <- model %>% fit(
    x = data %>% dplyr::select(-y),
    y = data$y,
    epochs = epochs,
    validation_split = 0.2,
    verbose = 0
  )  
  #plot(history)
  
  return(model)
}

tune.reg_cnn <- function (x, y = NULL, neurons, epochs) {
  tf$get_logger()$setLevel('ERROR')  
  ranges <- list(neurons = neurons)
  ranges <- expand.grid(ranges)
  n <- nrow(ranges)
  mses <- rep(0,n)
  data <- adjust.data.frame(cbind(x, y))
  folds <- k_fold(sample_random(), data, 3)
  
  i <- 1
  if (n > 1) {
    for (i in 1:n) {
      for (j in 1:3) {
        tt <- train_test_from_folds(folds, j)
        xx <- tt$train
        xx$y <- NULL
        yy <- tt$train$y
        model <- train.reg_cnn(x = xx, y = yy, neurons = ranges$neurons[i], epochs)
        xx <- tt$test
        xx$y <- NULL
        yy <- tt$test$y
        prediction <- predict(model, xx) 
        mses[i] <- mses[i] + regression_evaluation(yy, prediction)$mse
      }
    }
    i <- which.min(mses)
  }
  model <- train.reg_cnn(x = x, y = y, neurons = ranges$neurons[i], epochs)
  tf$get_logger()$setLevel('WARNING')  
  return(model)
}



# regression_evaluation
regression_evaluation <- function(values, prediction) {
  obj <- list(values=values, prediction=prediction)
  
  loadlibrary("TSPred")  
  
  obj$smape <- TSPred::sMAPE(values, prediction)  
  obj$mse <- TSPred::MSE(values, prediction)  
  
  obj$metrics <- data.frame(mse=obj$mse, smape=obj$smape)
  
  attr(obj, "class") <- "regression_evaluation"  
  return(obj)
}

