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



# regression_evaluation
regression_evaluation <- function(values, prediction) {
  obj <- list(values=values, prediction=prediction)
  
  loadlibrary("RSNNS")  
  loadlibrary("nnet")  
  loadlibrary("MLmetrics")  

  obj$mse <- (sum(values - prediction)^2)/length(values)
  
  obj$metrics <- data.frame(mse=obj$mse)

  attr(obj, "class") <- "regression_evaluation"  
  return(obj)
}

