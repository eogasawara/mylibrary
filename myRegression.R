# version 1.0
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myRegressionEvaluation.R")

#loadlibrary("kernlab")
#loadlibrary("rattle")
#loadlibrary("MASS")

# regression
regression <- function(data, attribute) {
  obj <- rel_transform(data)
  obj$attribute <- attribute
  obj$predictors <- setdiff(colnames(obj$data), attribute)  
  class(obj) <- append("regression", class(obj))    
  return(obj)
}

# regression_multiple 
regression_multiple <- function(data, attribute) {
  obj <- regression(data, attribute)
  class(obj) <- append("regression_multiple", class(obj))    
  return(obj)
}

prepare.regression_multiple <- function(obj) {
  #optimize it using lasso + anova
  obj <- start_log(obj)  
  return(obj)
}

action.regression_multiple  <- function(obj) {
  #pick code for multiple regression
  prediction <- rep(0, nrow(obj$data))
  return(prediction)
}

# decision_tree
regression_decision_tree <- function(data, attribute) {
  obj <- regression(data, attribute)
  class(obj) <- append("regression_decision_tree", class(obj))    
  return(obj)
}

prepare.regression_decision_tree <- function(obj) {
  obj <- start_log(obj)  
  
  loadlibrary("tree")
  
  regression <- formula(paste(obj$attribute, "  ~ ."))  
  obj$model <- tree(regression, obj$data)
  
  obj <- register_log(obj)
  return(obj)
}

action.regression_decision_tree <- function(obj) {
  predictors = obj$data[,obj$predictors]   
  prediction <- predict(obj$model, predictors, type="vector")  
  return(prediction)
}

# random_forest
regression_random_forest <- function(data, attribute, mtry = NULL, ntree = seq(50, 500, 50)) {
  obj <- regression(data, attribute)
  
  if (is.null(mtry))
    mtry <- unique(1:ceiling(ncol(data)/3))
  obj$mtry <- mtry
  obj$ntree <- ntree

  class(obj) <- append("regression_random_forest", class(obj))    
  return(obj)
}

prepare.regression_random_forest <- function(obj) {
  obj <- start_log(obj)  

  loadlibrary("e1071")
  loadlibrary("randomForest")
  
  regression <- formula(paste(obj$attribute, "  ~ ."))  
  tuned <- tune.randomForest(regression, data=obj$data, mtry=obj$mtry, ntree=obj$ntree)
  obj$model <- tuned$best.model 
  
  msg <- sprintf("mtry=%d,ntree=%d", obj$model$mtry, obj$model$ntree)
  obj <- register_log(obj, msg)
  return(obj)
}

action.regression_random_forest  <- function(obj) {
  predictors = obj$data[,obj$predictors]   
  prediction <- predict(obj$model, predictors)  
  return(prediction)
}

# mlp_nnet
regression_mlp_nnet <- function(data, attribute, neurons=NULL, decay=seq(0, 1, 0.02), maxit=1000) {
  obj <- regression(data, attribute)
  obj$maxit <- maxit
  if (is.null(neurons))
    neurons <- unique(1:ceiling(ncol(data)/3))
  obj$neurons <- neurons
  obj$decay <- decay
  
  class(obj) <- append("regression_mlp_nnet", class(obj))    
  return(obj)
}

prepare.regression_mlp_nnet <- function(obj) {
  obj <- start_log(obj)  
  
  loadlibrary("e1071")
  loadlibrary("nnet")
  
  regression <- formula(paste(obj$attribute, "  ~ ."))  
  tuned <- tune.nnet(regression, data=obj$data, trace=FALSE, maxit=obj$maxit, decay = obj$decay, size=obj$neurons, linout=TRUE)
  obj$model <- tuned$best.model  
  
  msg <- sprintf("neurons=%d,decay=%.2f", tuned$best.parameters$size, tuned$best.parameters$decay)
  obj <- register_log(obj, msg)
  return(obj)
}

action.regression_mlp_nnet  <- function(obj) {
  predictors = obj$data[,obj$predictors]   
  prediction <- predict(obj$model, predictors)  
  return(prediction)
}

# regression_svm 
regression_svm <- function(data, attribute, epsilon=seq(0,1,0.1), cost=seq(0,100,5), kernel="radial") {
  #kernel: linear, radial, polynomial, sigmoid
  #analisar: https://rpubs.com/Kushan/296706  
  obj <- regression(data, attribute)
  obj$kernel <- kernel
  obj$epsilon <- epsilon
  obj$cost <- cost
  
  class(obj) <- append("regression_svm", class(obj))    
  return(obj)
}

prepare.regression_svm <- function(obj) {
  obj <- start_log(obj)  
  
  loadlibrary("e1071")
  msg <- ""
  regression <- formula(paste(obj$attribute, "  ~ ."))  
  if (obj$kernel == "radial2") {
    tuned <- tune.svm(regression, data=obj$data, gamma=obj$gamma, cost=obj$cost, kernel="radial")
    obj$model <- tuned$best.model  
    msg <- sprintf("epsilon=%.1f,gamma=%.2f,cost=%.3f", obj$model$epsilon, obj$model$gamma, obj$model$cost)
  }
  if (obj$kernel == "radial") {
    tuned <- tune.svm(regression, data=obj$data, epsilon=obj$epsilon, cost=obj$cost, kernel="radial")
    obj$model <- tuned$best.model  
    msg <- sprintf("epsilon=%.1f,cost=%.3f", obj$model$epsilon, obj$model$cost)
  }
  
  obj <- register_log(obj, msg)
  return(obj)
}

action.regression_svm  <- function(obj) {
  predictors = obj$data[,obj$predictors]   
  prediction <- predict(obj$model, predictors) 
  return(prediction)
}

# regression_knn 
regression_knn <- function(data, attribute, k=1:20) {
  obj <- regression(data, attribute)
  obj$k <- k
  class(obj) <- append("regression_knn", class(obj))    
  return(obj)
}

prepare.regression_knn <- function(obj) {
  obj <- start_log(obj)  
  msg <- ""
  obj <- register_log(obj, msg)
  return(obj)
}

action.regression_knn  <- function(obj) {
  #develop from FNN https://daviddalpiaz.github.io/r4sl/knn-reg.html  
  prediction <- rep(0, nrow(obj$data))
  return(prediction)
}