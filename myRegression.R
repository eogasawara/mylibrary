# version 1.0
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myRegressionEvaluation.R")

#loadlibrary("kernlab")
#loadlibrary("rattle")
#loadlibrary("MASS")

# classif
regression <- function(data, attribute) {
  obj <- rel_transform(data)
  obj$attribute <- attribute
  obj$predictors <- setdiff(colnames(obj$data), attribute)  
  class(obj) <- append("regression", class(obj))    
  return(obj)
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
  obj$ntree <- ntree
  obj$mtry <- unique(c(2,2:round(max(sqrt(ncol(data)),ncol(data)/3))))
  class(obj) <- append("regression_random_forest", class(obj))    
  return(obj)
}

prepare.regression_random_forest <- function(obj) {
  obj <- start_log(obj)  

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
regression_mlp_nnet <- function(data, attribute, neurons=NULL, decay=seq(0, 1, 0.05), maxit=10000) {
  obj <- regression(data, attribute)
  obj$maxit <- maxit
  obj$decay <- decay
  obj$neurons <- 1:length(obj$predictors)
  if (!is.null(neurons))
    obj$neurons <- neurons
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
  
  msg <- sprintf("neurons=%d,decay=%.2f", obj$model$size, obj$model$decay)
  obj <- register_log(obj, msg)
  return(obj)
}

action.regression_mlp_nnet  <- function(obj) {
  predictors = obj$data[,obj$predictors]   
  prediction <- predict(obj$model, predictors)  
  return(prediction)
}

# regression_svm 
regression_svm <- function(data, attribute, epsilon=seq(0,1,0.1), cost=c(1, seq(2,100,2)), kernel="radial") {
  #kernel: linear, radial, polynomial, sigmoid
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
  
  regression <- formula(paste(obj$attribute, "  ~ ."))  
  tuned <- tune.svm(regression, data=obj$data, epsilon=obj$epsilon, cost=obj$cost, kernel=obj$kernel)
  obj$model <- tuned$best.model  
  
  msg <- sprintf("epsilon=%.1f,cost=%d", obj$model$epsilon, obj$model$cost)
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
  
  loadlibrary("e1071")
  
  predictors = obj$data[,obj$predictors] 
  predictand = obj$data[,obj$attribute]
  tuned <- tune.knn(x = predictors, y = predictand, k = obj$k)  
  obj$model <- list(predictors=predictors, predictand=predictand)
  obj$k <- tuned$k
  
  msg <- sprintf("k=%d", obj$k)
  obj <- register_log(obj, msg)
  return(obj)
}

action.regression_knn  <- function(obj) {
  loadlibrary("class")
  prediction = knn(train=obj$model$predictors, test=obj$data[,obj$predictors], cl=obj$model$predictand, prob=TRUE)
  prediction = decodeClassLabels(prediction)  
  return(prediction)
}