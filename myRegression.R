# version 1.0
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myTransform.R")

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
  obj <- prepare.regression(obj, data)  
  
  loadlibrary("tree")
  
  regression <- formula(paste(obj$attribute, "  ~ ."))  
  obj$model <- tree(regression, data)
  
  obj <- register_log(obj)
  return(obj)
}

action.reg_dtree <- function(obj, data) {
  predictors = data[,obj$predictors]   
  prediction <- predict(obj$model, predictors, type="vector")  
  return(prediction)
}

# random_forest
reg_rf <- function(attribute, mtry = NULL, ntree = seq(50, 500, 50)) {
  obj <- regression(attribute)
  
  if (is.null(mtry))
    mtry <- unique(1:ceiling(ncol(data)/3))
  obj$mtry <- mtry
  obj$ntree <- ntree

  class(obj) <- append("reg_rf", class(obj))    
  return(obj)
}

prepare.reg_rf <- function(obj, data) {
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
  predictors = data[,obj$predictors]   
  prediction <- predict(obj$model, predictors)  
  return(prediction)
}

# mlp_nnet
reg_mlp <- function(attribute, neurons=NULL, decay=seq(0, 1, 0.02), maxit=1000) {
  obj <- regression(attribute)
  obj$maxit <- maxit
  if (is.null(neurons))
    neurons <- unique(1:ceiling(ncol(data)/3))
  obj$neurons <- neurons
  obj$decay <- decay
  
  class(obj) <- append("reg_mlp", class(obj))    
  return(obj)
}

prepare.reg_mlp <- function(obj, data) {
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
  predictors = data[,obj$predictors]   
  prediction <- predict(obj$model, predictors)  
  return(prediction)
}

# reg_svm 
reg_svm <- function(attribute, epsilon=seq(0,1,0.1), cost=seq(5,100,5), kernel="radial") {
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
  predictors = data[,obj$predictors]   
  prediction <- predict(obj$model, predictors) 
  return(prediction)
}

# reg_knn 
reg_knn <- function(attribute, k=1:20) {
  obj <- regression(attribute)
  obj$k <- k
  
  class(obj) <- append("reg_knn", class(obj))    
  return(obj)
}

prepare.reg_knn <- function(obj, data) {
  obj <- prepare.regression(obj, data)  
  
  obj <- register_log(obj)
  return(obj)
}

action.reg_knn  <- function(obj, data) {
  #develop from FNN https://daviddalpiaz.github.io/r4sl/knn-reg.html  
  prediction <- rep(0, nrow(data))
  return(prediction)
}


# reg_cnn 

reg_cnn <- function(attribute, neurons=64, epochs = 1000) {
  obj <- regression(attribute)
  obj$neurons <- neurons
  obj$epochs <- epochs
  
  class(obj) <- append("reg_cnn", class(obj))    
  return(obj)
}

prepare.reg_cnn <- function(obj, data) {
  obj <- prepare.regression(obj, data)  
  
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
  
  data$y <- data[, obj$attribute]
  data[, obj$attribute] <- NULL
  
  set.seed(1)
  
  spec <- feature_spec(data, y ~ . ) %>% 
    step_numeric_column(all_numeric(), normalizer_fn = scaler_standard()) %>% 
    fit()
  
  input <- layer_input_from_dataset(data %>% dplyr::select(-y))
  
  output <- input %>% 
    layer_dense_features(dense_features(spec)) %>% 
    layer_dense(units = obj$neurons, activation = "relu") %>% 
    layer_dense(units = obj$neurons, activation = "relu") %>%
    layer_dense(units = 1) 
  
  model <- keras_model(input, output)
  
  model %>% 
    compile(loss = "mse", optimizer = optimizer_rmsprop(), 
            metrics = list("mean_absolute_error"))
  
  history <- model %>% fit(
    x = data %>% dplyr::select(-y),
    y = data$y,
    epochs = obj$epochs,
    validation_split = 0.2,
    verbose = 0,
    callbacks = list(print_dot_callback)
  )  
  cat("\n")
  
  obj$mdl <- model
  
  obj <- register_log(obj)
  return(obj)
}

action.reg_cnn  <- function(obj, data) {
  prediction <- (obj$mdl %>% predict(data))  
  return(prediction)
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

