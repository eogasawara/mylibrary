# version 1.0
source("myTransform.R")

# classif
classification <- function(attribute) {
  obj <- dal_transform()
  obj$attribute <- attribute
  
  class(obj) <- append("classification", class(obj))    
  return(obj)
}

prepare.classification <- function(obj, data) {
  obj <- start_log(obj) 
  obj$predictors <- setdiff(colnames(data), obj$attribute)  
  return(obj)
}

# zero_rule
classif_zero_rule <- function(attribute) {
  obj <- classification(attribute)
  
  class(obj) <- append("classif_zero_rule", class(obj))    
  return(obj)
}

prepare.classif_zero_rule <- function(obj, data) {
  obj <- prepare.classification(obj, data)
  
  loadlibrary("RSNNS")
  data[,obj$attribute] = as.factor(data[,obj$attribute])
  predictand = decodeClassLabels(data[,obj$attribute])
  cols <- apply(predictand, 2, sum)
  col <- match(max(cols),cols)
  obj$model <- list(cols=cols, col=col)
  
  obj <- register_log(obj)  
  return(obj)
}

action.classif_zero_rule <- function(obj, data) {
  loadlibrary("Matrix")  
  rows <- nrow(data)
  cols <- length(obj$model$cols)
  prediction <- Matrix(rep.int(0, rows*cols), nrow=rows, ncol=cols)
  prediction[,obj$model$col] <- 1
  colnames(prediction) <- names(obj$model$cols)
  prediction <- as.matrix(prediction)
  return(prediction)
}

# decision_tree
classif_decision_tree <- function(attribute) {
  obj <- classification(attribute)
  
  class(obj) <- append("classif_decision_tree", class(obj))    
  return(obj)
}

prepare.classif_decision_tree <- function(obj, data) {
  obj <- prepare.classification(obj, data)
  
  loadlibrary("tree")
  
  regression <- formula(paste(obj$attribute, "  ~ ."))  
  obj$model <- tree(regression, data)
  
  obj <- register_log(obj)  
  return(obj)
}

action.classif_decision_tree <- function(obj, data) {
  predictors = data[,obj$predictors]   
  prediction <- predict(obj$model, predictors, type="vector")  
  return(prediction)
}

# naive_bayes
classif_naive_bayes <- function(attribute) {
  obj <- classification(attribute)
  
  class(obj) <- append("classif_naive_bayes", class(obj))    
  return(obj)
}

prepare.classif_naive_bayes <- function(obj, data) {
  obj <- prepare.classification(obj, data)
  
  loadlibrary("e1071")
  
  regression <- formula(paste(obj$attribute, "  ~ ."))  
  obj$model <- naiveBayes(regression, data, laplace=0)
  
  obj <- register_log(obj)
  return(obj)
}

action.classif_naive_bayes  <- function(obj, data) {
  predictors = data[,obj$predictors]   
  prediction <- predict(obj$model, predictors, type="raw")  
  return(prediction)
}

# random_forest
classif_random_forest <- function(attribute, mtry = NULL, ntree = seq(50, 500, 50)) {
  obj <- classification(attribute)
  obj$ntree <- ntree
  
  class(obj) <- append("classif_random_forest", class(obj))    
  return(obj)
}

prepare.classif_random_forest <- function(obj, data) {
  obj <- prepare.classification(obj, data)
  
  if (is.null(obj$mtry))
    obj$mtry <- unique(2:round(sqrt(ncol(data)))) 
  
  loadlibrary("randomForest")
  
  regression <- formula(paste(obj$attribute, "  ~ ."))  
  tuned <- tune.randomForest(regression, data=data, mtry=obj$mtry, ntree=obj$ntree)
  obj$model <- tuned$best.model 
  
  msg <- sprintf("mtry=%d,ntree=%d", obj$model$mtry, obj$model$ntree)
  obj <- register_log(obj, msg)
  return(obj)
}

action.classif_random_forest  <- function(obj, data) {
  predictors = data[,obj$predictors]   
  prediction <- predict(obj$model, predictors, type="prob")  
  return(prediction)
}

# mlp_nnet
classif_mlp_nnet <- function(attribute, neurons=NULL, decay=seq(0, 1, 0.02), maxit=1000) {
  obj <- classification(attribute)
  obj$maxit <- maxit
  obj$neurons <- neurons
  obj$decay <- decay
  
  class(obj) <- append("classif_mlp_nnet", class(obj))    
  return(obj)
}

prepare.classif_mlp_nnet <- function(obj, data) {
  obj <- prepare.classification(obj, data)
  
  if (is.null(obj$neurons))
    obj$neurons <- unique(1:ceiling(sqrt(ncol(data))))
  
  
  loadlibrary("nnet")
  loadlibrary("e1071")
  
  regression <- formula(paste(obj$attribute, "  ~ ."))  
  tuned <- tune.nnet(regression, data=data, trace=FALSE, maxit=obj$maxit, decay = obj$decay, size=obj$neurons)
  obj$model <- tuned$best.model  
  
  msg <- sprintf("neurons=%d,decay=%.2f", tuned$best.parameters$size, tuned$best.parameters$decay)
  obj <- register_log(obj, msg)
  return(obj)
}

action.classif_mlp_nnet  <- function(obj, data) {
  predictors = data[,obj$predictors]   
  prediction <- predict(obj$model, predictors, type="raw")  
  return(prediction)
}

# classif_svm 
classif_svm <- function(attribute, epsilon=seq(0,1,0.1), cost=seq(5,100,5), kernel="radial") {
  #kernel: linear, radial, polynomial, sigmoid
  #analisar: https://rpubs.com/Kushan/296706  
  obj <- classification(attribute)
  obj$kernel <- kernel
  obj$epsilon <- epsilon
  obj$cost <- cost
  
  class(obj) <- append("classif_svm", class(obj))    
  return(obj)
}

prepare.classif_svm <- function(obj, data) {
  obj <- prepare.classification(obj, data)
  
  loadlibrary("e1071")
  
  regression <- formula(paste(obj$attribute, "  ~ ."))  
  tuned <- tune.svm(regression, data=data, probability=TRUE, epsilon=obj$epsilon, cost=obj$cost, kernel=obj$kernel)
  obj$model <- tuned$best.model  
  
  msg <- sprintf("epsilon=%.1f,cost=%.3f", obj$model$epsilon, obj$model$cost)
  obj <- register_log(obj, msg)
  return(obj)
}

action.classif_svm  <- function(obj, data) {
  predictors = data[,obj$predictors]   
  prediction <- predict(obj$model, predictors, probability = TRUE) 
  prediction <- attr(prediction, "probabilities")
  return(prediction)
}

# classif_knn 
classif_knn <- function(attribute, k=1:20) {
  obj <- classification(attribute)
  obj$k <- k
  class(obj) <- append("classif_knn", class(obj))    
  return(obj)
}

prepare.classif_knn <- function(obj, data) {
  obj <- prepare.classification(obj, data)
  
  loadlibrary("e1071")
  loadlibrary("class")
  
  predictors = data[,obj$predictors] 
  predictand = data[,obj$attribute]
  tuned <- tune.knn(x = predictors, y = predictand, k = obj$k)  
  obj$model <- list(predictors=predictors, predictand=predictand)
  obj$k <- tuned$k
  
  msg <- sprintf("k=%d", obj$k)
  obj <- register_log(obj)
  return(obj)
}

action.classif_knn  <- function(obj, data) {
  loadlibrary("class")
  prediction = knn(train=obj$model$predictors, test=data[,obj$predictors], cl=obj$model$predictand, prob=TRUE)
  prediction = decodeClassLabels(prediction)  
  return(prediction)
}


#classif_evaluation

classif_evaluation <- function(data, prediction) {
  obj <- list(data=data, prediction=prediction)
  
  loadlibrary("RSNNS")  
  loadlibrary("nnet")  
  loadlibrary("MLmetrics")  
  adjust_predictions <- function(predictions) {
    predictions_i = as.matrix(Matrix(rep.int(0, nrow(predictions)*ncol(predictions)), nrow=nrow(predictions), ncol=ncol(predictions)))
    y <- apply(predictions, 1, which.is.max)
    for(i in unique(y)) {
      predictions_i[y==i,i] <- 1  
    }
    return(predictions_i)
  }
  predictions <- adjust_predictions(obj$prediction)
  obj$conf_mat = RSNNS::confusionMatrix(data, predictions)
  obj$accuracy <- Accuracy(y_pred = predictions, y_true = data)
  obj$f1 <- F1_Score(y_pred = predictions, y_true = data, positive = 1)
  obj$sensitivity <- Sensitivity(y_pred = predictions, y_true = data, positive = 1)
  obj$specificity <- Specificity(y_pred = predictions, y_true = data, positive = 1)
  obj$precision <- Precision(y_pred = predictions, y_true = data, positive = 1)
  obj$recall <- Recall(y_pred = predictions, y_true = data, positive = 1)
  obj$metrics <- data.frame(accuracy=obj$accuracy, f1=obj$f1, sensitivity=obj$sensitivity, specificity=obj$specificity, precision=obj$precision, recall=obj$recall)
  
  attr(obj, "class") <- "classif_evaluation"  
  return(obj)
}

roc_curve <- function(obj) {
  UseMethod("roc_curve")
}

roc_curve.classif_evaluation <- function(obj) {
  loadlibrary("ROCR")
  pred <- prediction(obj$prediction, obj$data)
  rocr <- performance(pred, "tpr", "fpr")  
  return (rocr)  
}