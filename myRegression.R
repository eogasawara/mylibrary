# version 1.0
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myTransform.R")
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/mySample.R")

# regression
regression <- function(attribute) {
  obj <- dal_transform()
  obj$attribute <- attribute
  
  class(obj) <- append("regression", class(obj))    
  return(obj)
}

prepare.regression <- function(obj, data) {
  obj <- start_log(obj) 
  obj$x <- setdiff(colnames(data), obj$attribute)  
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
  x <- data[,obj$x]   
  prediction <- predict(obj$model, x, type="vector")  
  return(prediction)
}

# random_forest
reg_rf <- function(attribute, mtry = NULL, ntree = seq(5, 50, 5)) {
  obj <- regression(attribute)
  
  obj$mtry <- mtry
  obj$ntree <- ntree
  
  class(obj) <- append("reg_rf", class(obj))    
  return(obj)
}

prepare.reg_rf <- function(obj, data) {
  data <- adjust.data.frame(data)
  obj <- prepare.regression(obj, data)  
  
  loadlibrary("randomForest")
  
  if (is.null(obj$mtry))
    obj$mtry <- ceiling(ncol(data)/3)
  
  x <- data[,obj$x]
  y <- data[,obj$attribute]
  
  obj$model <- tune.reg_rf(x=x, y=y, mtry=obj$mtry, ntree=obj$ntree)
  
  params <- attr(obj$model, "params") 
  msg <- sprintf("mtry=%d,ntree=%d", params$mtry, params$ntree)
  obj <- register_log(obj, msg)
  return(obj)
}

action.reg_rf  <- function(obj, data) {
  data <- adjust.data.frame(data)
  x <- data[,obj$x]   
  prediction <- predict(obj$model, x)  
  return(prediction)
}

tune.reg_rf <- function (x, y, mtry, ntree) {
  ranges <- list(mtry = mtry, ntree = ntree)
  model <- tune.general.regression(x = x, y = y, ranges = ranges, train.func = randomForest)
  return(model)
}


# mlp_nnet
reg_mlp <- function(attribute, size=NULL, decay=seq(0, 1, 0.1), maxit=1000) {
  obj <- regression(attribute)
  obj$maxit <- maxit
  obj$size <- size
  obj$decay <- decay
  
  class(obj) <- append("reg_mlp", class(obj))    
  return(obj)
}

prepare.reg_mlp <- function(obj, data) {
  data <- adjust.data.frame(data)
  obj <- prepare.regression(obj, data)  
  
  loadlibrary("e1071")
  loadlibrary("nnet")
  
  if (is.null(obj$size))
    obj$size <- ceiling(ncol(data)/3)
  
  x <- data[,obj$x]
  y <- data[,obj$attribute]
  obj$model <- tune.reg_mlp(x, y, size = obj$size, decay = obj$decay, maxit=obj$maxit)
  
  params <- attr(obj$model, "params") 
  msg <- sprintf("size=%d,decay=%.2f", params$size, params$decay)
  obj <- register_log(obj, msg)
  return(obj)
}

action.reg_mlp  <- function(obj, data) {
  data <- adjust.data.frame(data)
  x <- data[,obj$x]   
  prediction <- predict(obj$model, x)  
  return(prediction)
}

tune.reg_mlp <- function (x, y, size, decay, maxit) {
  ranges <- list(size = size, decay = decay, maxit = maxit, linout=TRUE, trace = FALSE)
  model <- tune.general.regression(x = x, y = y, ranges = ranges, train.func = nnet)
  return(model)
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
  
  #  loadlibrary("e1071")
  #  regression <- formula(paste(obj$attribute, "  ~ ."))  
  #  tuned <- tune.svm(regression, data=data, epsilon=obj$epsilon, cost=obj$cost, kernel="radial")
  #  obj$model <- tuned$best.model  
  #msg <- sprintf("epsilon=%.1f,cost=%.3f", obj$model$epsilon, obj$model$cost)
  
  x <- data[,obj$x]
  y <- data[,obj$attribute]
  obj$model <- tune.reg_svm(x, y, epsilon=obj$epsilon, cost=obj$cost, kernel="radial")
  
  params <- attr(obj$model, "params") 
  msg <- sprintf("epsilon=%.1f,cost=%.3f", params$epsilon, params$cost)
  obj <- register_log(obj, msg)
  return(obj)
}

action.reg_svm  <- function(obj, data) {
  data <- adjust.data.frame(data)
  x <- data[,obj$x]   
  prediction <- predict(obj$model, x) 
  return(prediction)
}

tune.reg_svm <- function (x, y, epsilon, cost, kernel) {
  ranges <- list(epsilon = epsilon, cost = cost, kernel = kernel)
  model <- tune.general.regression(x = x, y = y, ranges = ranges, train.func = svm)
  return(model)
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
  loadlibrary("FNN")
  
  x <- as.matrix(data[,obj$x])
  y <- data[,obj$attribute]
  
  obj$model <- tune.reg_knn(x=x, y=y, k = obj$k)  
  
  params <- attr(obj$model, "params") 
  msg <- sprintf("k=%d", params$k)
  obj <- register_log(obj, msg)
  return(obj)
}

action.reg_knn  <- function(obj, data) {
  #develop from FNN https://daviddalpiaz.github.io/r4sl/knn-reg.html
  data <- adjust.data.frame(data)
  x <- as.matrix(data[,obj$x])
  prediction <- predict.reg_knn(obj$model, x)
  return(prediction)
}


tune.reg_knn <- function (x, y, k) {
  ranges <- list(k = k, stub = 0)
  model <- tune.general.regression(x = x, y = y, ranges = ranges, train.func = train.reg_knn, pred.fun = predict.reg_knn)
  return(model)
}

train.reg_knn <- function (x, y, k, ...) {
  model <- list(x=x, y=y, k=k)
  return (model)
}

predict.reg_knn <- function(model, x) {
  prediction <- knn.reg(train = model$x, test = x, y = model$y, k = model$k)  
  return(prediction$pred)
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
  
  x <- data[obj$x]
  y <- data[,obj$attribute]
  
  obj$model <- tune.reg_cnn(x = x, y = y, neurons=obj$neurons, epochs=obj$epochs)
  
  params <- attr(obj$model, "params") 
  msg <- sprintf("neurons=%d,epochs=%d", params$neurons, params$epochs)
  obj <- register_log(obj, msg)
  return(obj)
}

action.reg_cnn  <- function(obj, data) {
  data <- adjust.data.frame(data)
  x <- data[,obj$x]   
  prediction <- predict(obj$model, x)
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

tune.reg_cnn <- function (x, y, neurons, epochs) {
  tf$get_logger()$setLevel('ERROR')
  ranges <- list(neurons = neurons, epochs = epochs)
  model <- tune.general.regression(x = x, y = y, ranges = ranges, train.func = train.reg_cnn)
  tf$get_logger()$setLevel('WARNING')
  return(model)
}

myRegRepro <- TRUE
reg_repro <- function() {
  if (myRegRepro)
    set.seed(1)
}

tune.general.regression <- function (x, y, ranges, folds=3, train.func, pred.fun = predict) {
  ranges <- expand.grid(ranges)
  n <- nrow(ranges)
  errors <- rep(0,n)
  data <- adjust.data.frame(cbind(x, y))
  folds <- k_fold(sample_random(), data, folds)
  
  i <- 1
  if (n > 1) {
    for (i in 1:n) {
      for (j in 1:length(folds)) {
        reg_repro()
        tt <- train_test_from_folds(folds, j)
        params <- append(list(x = tt$train[colnames(x)], y = tt$train$y), as.list(ranges[i,]))
        model <- do.call(train.func, params)
        prediction <- pred.fun(model, tt$test[colnames(x)]) 
        errors[i] <- errors[i] + regression_evaluation(tt$test$y, prediction)$mse 
      }
    }
    i <- which.min(errors)
  }
  params <- append(list(x = x, y = y), as.list(ranges[i,]))
  reg_repro()
  model <- do.call(train.func, params)
  attr(model, "params") <- as.list(ranges[i,])
  return(model)
}

reg.MSE <- function (actual, prediction) 
{
  if (length(actual) != length(prediction)) 
    stop("actual and prediction have different lengths")
  n <- length(actual)
  res <- mean((actual - prediction)^2)
  res
}

reg.sMAPE <- function (actual, prediction) 
{
  if (length(actual) != length(prediction)) 
    stop("actual and prediction have different lengths")
  n <- length(actual)
  res <- (1/n) * sum(abs(actual - prediction)/((abs(actual) + 
                                                  abs(prediction))/2))
  res
}



# regression_evaluation
regression_evaluation <- function(values, prediction) {
  obj <- list(values=values, prediction=prediction)
  
  obj$smape <- reg.sMAPE(values, prediction)  
  obj$mse <- reg.MSE(values, prediction)  
  
  obj$metrics <- data.frame(mse=obj$mse, smape=obj$smape)
  
  attr(obj, "class") <- "regression_evaluation"  
  return(obj)
}

