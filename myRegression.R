# version 1.5
# depends myBasic.R
# depends myPreprocessing.R

# regression
regression <- function(attribute) {
  obj <- list()
  attr(obj, "class") <- "regression"  
  obj$attribute <- attribute
  return(obj)
}

fit.regression <- function(obj, data) {
  obj <- start_log(obj) 
  obj$x <- setdiff(colnames(data), obj$attribute)  
  return(obj)
}

# decision_tree
regression_dtree <- function(attribute) {
  obj <- regression(attribute)
  
  class(obj) <- append("regression_dtree", class(obj))    
  return(obj)
}

fit.regression_dtree <- function(obj, data) {
  data <- adjust.data.frame(data)
  obj <- fit.regression(obj, data)  
  loadlibrary("tree")
  
  regression <- formula(paste(obj$attribute, "  ~ ."))  
  obj$model <- tree(regression, data)
  
  obj <- register_log(obj)
  return(obj)
}

predict.regression_dtree <- function(obj, x) {
  x <- adjust.data.frame(x)
  x <- x[,obj$x]   
  prediction <- predict(obj$model, x, type="vector")  
  return(prediction)
}

# random_forest
regression_rf <- function(attribute, mtry = NULL, ntree = seq(5, 50, 5)) {
  obj <- regression(attribute)
  
  obj$mtry <- mtry
  obj$ntree <- ntree
  
  class(obj) <- append("regression_rf", class(obj))    
  return(obj)
}

fit.regression_rf <- function(obj, data) {
  data <- adjust.data.frame(data)
  obj <- fit.regression(obj, data)  
  
  loadlibrary("randomForest")
  
  if (is.null(obj$mtry))
    obj$mtry <- ceiling(c(1,1.5,2)*ncol(data)/3)
  
  x <- data[,obj$x]
  y <- data[,obj$attribute]
  
  ranges <- list(mtry=obj$mtry, ntree=obj$ntree)
  obj$model <- tune.regression(x = x, y = y, ranges = ranges, fit.func = randomForest)

  params <- attr(obj$model, "params") 
  msg <- sprintf("mtry=%d,ntree=%d", params$mtry, params$ntree)
  obj <- register_log(obj, msg)
  return(obj)
}

predict.regression_rf  <- function(obj, x) {
  x <- adjust.data.frame(x)
  x <- x[,obj$x]   
  prediction <- predict(obj$model, x)  
  return(prediction)
}


# mlp_nnet
regression_mlp <- function(attribute, size=NULL, decay=seq(0, 1, 0.0335), maxit=1000) {
  obj <- regression(attribute)
  obj$maxit <- maxit
  obj$size <- size
  obj$decay <- decay
  class(obj) <- append("regression_mlp", class(obj))    
  return(obj)
}

fit.regression_mlp <- function(obj, data) {
  data <- adjust.data.frame(data)
  obj <- fit.regression(obj, data)  
  
  loadlibrary("nnet")
  
  if (is.null(obj$size))
    obj$size <- ceiling(ncol(data)/3)
  
  x <- data[,obj$x]
  y <- data[,obj$attribute]
  ranges <- list(size = obj$size, decay = obj$decay, maxit=obj$maxit, linout=TRUE, trace = FALSE)
  obj$model <- tune.regression(x = x, y = y, ranges = ranges, fit.func = nnet)

  params <- attr(obj$model, "params") 
  msg <- sprintf("size=%d,decay=%.2f", params$size, params$decay)
  obj <- register_log(obj, msg)
  return(obj)
}

predict.regression_mlp  <- function(obj, x) {
  x <- adjust.data.frame(x)
  x <- x[,obj$x]   
  prediction <- predict(obj$model, x)  
  return(prediction)
}


# regression_svm 
regression_svm <- function(attribute, epsilon=seq(0,1,0.2), cost=seq(20,100,20), kernel="radial") {
  #kernel: linear, radial, polynomial, sigmoid
  #analisar: https://rpubs.com/Kushan/296706  
  obj <- regression(attribute)
  obj$kernel <- kernel
  obj$epsilon <- epsilon
  obj$cost <- cost
  
  class(obj) <- append("regression_svm", class(obj))    
  return(obj)
}

fit.regression_svm <- function(obj, data) {
  data <- adjust.data.frame(data)
  obj <- fit.regression(obj, data)  
  
  loadlibrary("e1071")
  
  x <- data[,obj$x]
  y <- data[,obj$attribute]
  
  ranges <- list(epsilon=obj$epsilon, cost=obj$cost, kernel=obj$kernel)
  obj$model <- tune.regression(x = x, y = y, ranges = ranges, fit.func = svm)

  params <- attr(obj$model, "params") 
  msg <- sprintf("epsilon=%.1f,cost=%.3f", params$epsilon, params$cost)
  obj <- register_log(obj, msg)
  return(obj)
}

predict.regression_svm  <- function(obj, x) {
  x <- adjust.data.frame(x)
  x <- x[,obj$x]   
  prediction <- predict(obj$model, x) 
  return(prediction)
}

# regression_knn 
regression_knn <- function(attribute, k=1:30) {
  obj <- regression(attribute)
  obj$k <- k
  
  class(obj) <- append("regression_knn", class(obj))    
  return(obj)
}

fit.regression_knn <- function(obj, data) {
  internal_fit.regression_knn <- function (x, y, k, ...) {
    model <- list(x=x, y=y, k=k)
    return (model)
  }  
  
  internal_predict.regression_knn <- function(model, x) {
    prediction <- knn.reg(train = model$x, test = x, y = model$y, k = model$k)  
    return(prediction$pred)
  }  
  data <- adjust.data.frame(data)
  obj <- fit.regression(obj, data)  
  loadlibrary("FNN")
  
  x <- as.matrix(data[,obj$x])
  y <- data[,obj$attribute]
  
  ranges <- list(k = obj$k, stub = 0)
  obj$model <- tune.regression(x = x, y = y, ranges = ranges, fit.func = internal_fit.regression_knn, pred.fun = internal_predict.regression_knn)

  params <- attr(obj$model, "params") 
  msg <- sprintf("k=%d", params$k)
  obj <- register_log(obj, msg)
  return(obj)
}


predict.regression_knn  <- function(obj, x) {
  #develop from FNN https://daviddalpiaz.github.io/r4sl/knn-reg.html
  x <- adjust.data.frame(x)
  x <- as.matrix(x[,obj$x])
  prediction <- knn.reg(train = obj$model$x, test = x, y = obj$model$y, k = obj$model$k)  
  return(prediction$pred)
}

# regression_cnn 

regression_cnn <- function(attribute, neurons=c(3,5,10,16,32), epochs = c(100, 150, 200)) {
  obj <- regression(attribute)
  obj$neurons <- neurons
  obj$epochs <- epochs
  
  class(obj) <- append("regression_cnn", class(obj))    
  return(obj)
}

fit.regression_cnn <- function(obj, data) {
  internal_fit.regression_cnn <- function(x, y, neurons, epochs, ...) {
    data <- adjust.data.frame(x)
    data$y <- y
    
    spec <- feature_spec(data, y ~ . ) %>% 
      step_numeric_column(all_numeric(), normalizer_fn = scaler_standard()) %>% 
      keras::fit()
    
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
    
    
    history <- model %>% keras::fit(
      x = data %>% dplyr::select(-y),
      y = data$y,
      epochs = epochs,
      validation_split = 0.2,
      verbose = 0
    )  
    #plot(history)
    
    return(model)
  }
  
  data <- adjust.data.frame(data)
  obj <- fit.regression(obj, data)  
  
  loadlibrary("dplyr")
  loadlibrary("tfdatasets")
  loadlibrary("tensorflow")
  loadlibrary("keras")  
  
  x <- data[obj$x]
  y <- data[,obj$attribute]
  
  tf$get_logger()$setLevel('ERROR')
  ranges <- list(neurons=obj$neurons, epochs=obj$epochs)
  obj$model <- tune.regression(x = x, y = y, ranges = ranges, fit.func = internal_fit.regression_cnn)
  tf$get_logger()$setLevel('WARNING')

  params <- attr(obj$model, "params") 
  msg <- sprintf("neurons=%d,epochs=%d", params$neurons, params$epochs)
  obj <- register_log(obj, msg)
  return(obj)
}


predict.regression_cnn  <- function(obj, x) {
  loadlibrary("tensorflow")
  loadlibrary("keras")  

  x <- adjust.data.frame(x)
  x <- x[,obj$x]   
  prediction <- predict(obj$model, x)
  return(prediction)
}

reg.reproduce <- TRUE
regression.check_reproduce <- function() {
  if (reg.reproduce)
    set.seed(1)
}

tune.regression <- function (x, y, ranges, folds=3, fit.func, pred.fun = predict) {
  ranges <- expand.grid(ranges)
  n <- nrow(ranges)
  errors <- rep(0,n)
  
  data <- data.frame(i = 1:nrow(x), idx = 1:nrow(x))
  folds <- k_fold(sample_random(), data, folds)

  i <- 1
  if (n > 1) {
    for (i in 1:n) {
      for (j in 1:length(folds)) {
        regression.check_reproduce()
        tt <- train_test_from_folds(folds, j)
        
        params <- append(list(x = x[tt$train$i,], y = y[tt$train$i]), as.list(ranges[i,]))
        model <- do.call(fit.func, params)
        prediction <- pred.fun(model, x[tt$test$i,]) 
        errors[i] <- errors[i] + evaluation.regression(y[tt$test$i], prediction)$mse 
      }
    }
    i <- which.min(errors)
  }
  params <- append(list(x = x, y = y), as.list(ranges[i,]))
  regression.check_reproduce()
  model <- do.call(fit.func, params)
  attr(model, "params") <- as.list(ranges[i,])
  return(model)
}

MSE.regression <- function (actual, prediction) 
{
  if (length(actual) != length(prediction)) 
    stop("actual and prediction have different lengths")
  n <- length(actual)
  res <- mean((actual - prediction)^2)
  res
}

sMAPE.regression <- function (actual, prediction) 
{
  if (length(actual) != length(prediction)) 
    stop("actual and prediction have different lengths")
  n <- length(actual)
  res <- (1/n) * sum(abs(actual - prediction)/((abs(actual) + 
                                                  abs(prediction))/2))
  res
}



# evaluation.regression
evaluation.regression <- function(values, prediction) {
  obj <- list(values=values, prediction=prediction)
  
  obj$smape <- sMAPE.regression(values, prediction)  
  obj$mse <- MSE.regression(values, prediction)  
  
  obj$metrics <- data.frame(mse=obj$mse, smape=obj$smape)
  
  attr(obj, "class") <- "evaluation.regression"  
  return(obj)
}

