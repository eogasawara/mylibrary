# version 1.0
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myTransform.R")

# classif
classification <- function(attribute, slevels=NULL) {
  obj <- dal_transform()
  obj$attribute <- attribute
  obj$slevels <- slevels
  obj$ilevels <- 1:length(slevels)
  
  class(obj) <- append("classification", class(obj))    
  return(obj)
}


adjust.factor <- function(value, ilevels, slevels) {
  if (!is.factor(value)) {
    if (is.numeric(value))
      value <- factor(value, levels=ilevels)
      levels(value) <- slevels
  }
  return(value)
}

prepare.classification <- function(obj, data) {
  obj <- start_log(obj) 
  obj$predictors <- setdiff(colnames(data), obj$attribute)  
  return(obj)
}

# zero_rule
class_majority <- function(attribute, slevels=NULL) {
  obj <- classification(attribute, slevels)
  
  class(obj) <- append("class_majority", class(obj))    
  return(obj)
}

prepare.class_majority <- function(obj, data) {
  data <- adjust.data.frame(data)
  data[,obj$attribute] <- adjust.factor(data[,obj$attribute], obj$ilevels, obj$slevels)
  obj <- prepare.classification(obj, data)
  
  loadlibrary("RSNNS")
  predictand <- decodeClassLabels(data[,obj$attribute])
  cols <- apply(predictand, 2, sum)
  col <- match(max(cols),cols)
  obj$model <- list(cols=cols, col=col)
  
  obj <- register_log(obj)  
  return(obj)
}

action.class_majority <- function(obj, data) {
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
class_dtree <- function(attribute, slevels=NULL) {
  obj <- classification(attribute, slevels)
  
  class(obj) <- append("class_dtree", class(obj))    
  return(obj)
}

prepare.class_dtree <- function(obj, data) {
  data <- adjust.data.frame(data)
  data[,obj$attribute] <- adjust.factor(data[,obj$attribute], obj$ilevels, obj$slevels)
  obj <- prepare.classification(obj, data)
  loadlibrary("tree")
  
  regression <- formula(paste(obj$attribute, "  ~ ."))  
  obj$model <- tree(regression, data)
  
  obj <- register_log(obj)  
  return(obj)
}

action.class_dtree <- function(obj, data) {
  data <- adjust.data.frame(data)
  predictors <- data[,obj$predictors]   
  prediction <- predict(obj$model, predictors, type="vector")  
  return(prediction)
}

# naive_bayes
class_nb <- function(attribute, slevels=NULL) {
  obj <- classification(attribute, slevels)
  
  class(obj) <- append("class_nb", class(obj))    
  return(obj)
}

prepare.class_nb <- function(obj, data) {
  data <- adjust.data.frame(data)
  data[,obj$attribute] <- adjust.factor(data[,obj$attribute], obj$ilevels, obj$slevels)
  obj <- prepare.classification(obj, data)
  loadlibrary("e1071")
  
  regression <- formula(paste(obj$attribute, "  ~ ."))  
  obj$model <- naiveBayes(regression, data, laplace=0)
  
  obj <- register_log(obj)
  return(obj)
}

action.class_nb  <- function(obj, data) {
  data <- adjust.data.frame(data)
  predictors <- data[,obj$predictors]
  prediction <- predict(obj$model, predictors, type="raw")
  return(prediction)
}

# random_forest
class_rf <- function(attribute, slevels=NULL, mtry = NULL, ntree = seq(5, 50, 5)) {
  obj <- classification(attribute, slevels)
  obj$ntree <- ntree
  obj$mtry <- mtry
  class(obj) <- append("class_rf", class(obj))
  return(obj)
}

prepare.class_rf <- function(obj, data) {
  data <- adjust.data.frame(data)
  data[,obj$attribute] <- adjust.factor(data[,obj$attribute], obj$ilevels, obj$slevels)
  obj <- prepare.classification(obj, data)
  loadlibrary("randomForest")
  
  if (is.null(obj$mtry))
    obj$mtry <- unique(2:round(sqrt(ncol(data))))
  
  tuned <- tune.randomForest(x=data[, obj$predictors], y=data[,obj$attribute], mtry=obj$mtry, ntree=obj$ntree)
  obj$model <- tuned$best.model
  
  msg <- sprintf("mtry=%d,ntree=%d", obj$model$mtry, obj$model$ntree)
  obj <- register_log(obj, msg)
  return(obj)
}

action.class_rf  <- function(obj, data) {
  data <- adjust.data.frame(data)
  predictors <- data[,obj$predictors]   
  prediction <- predict(obj$model, predictors, type="prob")  
  return(prediction)
}

# mlp_nnet
class_mlp <- function(attribute, slevels=NULL, neurons=c(2, 4, 6), decay=seq(0, 1, 0.2), maxit=1000) {
  obj <- classification(attribute, slevels)
  obj$maxit <- maxit
  obj$neurons <- neurons
  obj$decay <- decay
  
  class(obj) <- append("class_mlp", class(obj))    
  return(obj)
}

prepare.class_mlp <- function(obj, data) {
  data <- adjust.data.frame(data)
  data[,obj$attribute] <- adjust.factor(data[,obj$attribute], obj$ilevels, obj$slevels)
  obj <- prepare.classification(obj, data)
  loadlibrary("nnet")
  loadlibrary("e1071")
  
  if (is.null(obj$neurons))
    obj$neurons <- unique(1:ceiling(sqrt(ncol(data))))

  regression <- formula(paste(obj$attribute, "  ~ ."))  
  tuned <- tune.nnet(regression, data=data, trace=FALSE, maxit=obj$maxit, decay = obj$decay, size=obj$neurons, MaxNWts=5000)
  obj$model <- tuned$best.model  
  
  msg <- sprintf("neurons=%d,decay=%.2f", tuned$best.parameters$size, tuned$best.parameters$decay)
  obj <- register_log(obj, msg)
  return(obj)
}

action.class_mlp  <- function(obj, data) {
  data <- adjust.data.frame(data)
  predictors <- data[,obj$predictors]   
  prediction <- predict(obj$model, predictors, type="raw")  
  return(prediction)
}

# class_svm 
class_svm <- function(attribute, slevels=NULL, epsilon=seq(0,1,0.1), cost=seq(5,100,5), kernel="radial") {
  #kernel: linear, radial, polynomial, sigmoid
  #analisar: https://rpubs.com/Kushan/296706  
  obj <- classification(attribute, slevels)
  obj$kernel <- kernel
  obj$epsilon <- epsilon
  obj$cost <- cost
  
  class(obj) <- append("class_svm", class(obj))    
  return(obj)
}

prepare.class_svm <- function(obj, data) {
  data <- adjust.data.frame(data)
  data[,obj$attribute] <- adjust.factor(data[,obj$attribute], obj$ilevels, obj$slevels)
  obj <- prepare.classification(obj, data)
  loadlibrary("e1071")
  
  regression <- formula(paste(obj$attribute, "  ~ ."))  
  tuned <- tune.svm(regression, data=data, probability=TRUE, epsilon=obj$epsilon, cost=obj$cost, kernel=obj$kernel)
  obj$model <- tuned$best.model  
  
  msg <- sprintf("epsilon=%.1f,cost=%.3f", obj$model$epsilon, obj$model$cost)
  obj <- register_log(obj, msg)
  return(obj)
}

action.class_svm  <- function(obj, data) {
  data <- adjust.data.frame(data)
  predictors <- data[,obj$predictors]   
  prediction <- predict(obj$model, predictors, probability = TRUE) 
  prediction <- attr(prediction, "probabilities")
  prediction <- prediction[,obj$slevels]
  return(prediction)
}

# class_knn 
class_knn <- function(attribute, slevels=NULL, k=seq(1, 20, 4)) {
  obj <- classification(attribute, slevels)
  obj$k <- k
  class(obj) <- append("class_knn", class(obj))    
  return(obj)
}

prepare.class_knn <- function(obj, data) {
  data <- adjust.data.frame(data)
  data[,obj$attribute] <- adjust.factor(data[,obj$attribute], obj$ilevels, obj$slevels)
  obj <- prepare.classification(obj, data)
  
  loadlibrary("e1071")
  loadlibrary("class")
  
  predictors <- data[,obj$predictors]
  predictand <- data[,obj$attribute]
  tuned <- tune.knn(x=predictors, y=predictand, k = obj$k)  
  obj$model <- list(predictors=predictors, predictand=predictand)
  obj$k <- tuned$k
  
  msg <- sprintf("k=%d", obj$k)
  obj <- register_log(obj)
  return(obj)
}

action.class_knn  <- function(obj, data) {
  data <- adjust.data.frame(data)
  predictors <- data[,obj$predictors]   
  loadlibrary("class")
  prediction <- knn(train=obj$model$predictors, test=predictors, cl=obj$model$predictand, prob=TRUE)
  prediction <- decodeClassLabels(prediction)  
  return(prediction)
}

# class_cnn 
class_cnn <- function(attribute, slevels=NULL, neurons=64, epochs = 100) {
  obj <- classification(attribute, slevels)
  obj$neurons <- neurons
  obj$epochs <- epochs
  
  class(obj) <- append("class_cnn", class(obj))    
  return(obj)
}

prepare.class_cnn <- function(obj, data) {
  data <- adjust.data.frame(data)
  data[,obj$attribute] <- adjust.factor(data[,obj$attribute], obj$ilevels, obj$slevels)
  obj <- prepare.classification(obj, data)
  loadlibrary("dplyr")
  loadlibrary("tfdatasets")
  loadlibrary("tensorflow")
  loadlibrary("keras")  
  
  predictand <- to_categorical(as.numeric(data[,obj$attribute]) - 1)
  predictors <- data[,obj$predictors]
  predictors <- as.matrix(predictors)
  predictand <- as.matrix(predictand)

  model <- keras_model_sequential()
  
  model %>%
    layer_dense(units = ncol(predictand), activation = 'softmax',
                input_shape = ncol(predictors))
  summary(model)
  
  sgd <- optimizer_sgd(lr = 0.01)
  
  model %>% compile(
    loss = 'categorical_crossentropy',
    optimizer = sgd,
    metrics = 'accuracy'
  )
  
  history <- model %>% fit(
    x = predictors,
    y = predictand,
    epochs = obj$epochs,
    batch_size = 5,
    validation_split = 0.2,
    verbose = 0
  )
  plot(history)  
  
  obj$mdl <- model
  
  obj <- register_log(obj)
  return(obj)
}

action.class_cnn  <- function(obj, data) {
  data <- adjust.data.frame(data)
  predictors <- data[,obj$predictors]   
  predictors <- as.matrix(predictors)
  prediction <- obj$mdl %>% predict_classes(predictors)
  prediction <- factor(prediction)
  levels(prediction) <- obj$slevels
  prediction <- decodeClassLabels(prediction)
  return(prediction)
}



#classif_evaluation

classif_evaluation <- function(data, prediction) {
  obj <- list(data=data, prediction=prediction)
  
  loadlibrary("RSNNS")  
  loadlibrary("nnet")  
  loadlibrary("MLmetrics")  
  adjust_predictions <- function(predictions) {
    predictions_i <- as.matrix(Matrix(rep.int(0, nrow(predictions)*ncol(predictions)), nrow=nrow(predictions), ncol=ncol(predictions)))
    y <- apply(predictions, 1, which.is.max)
    for(i in unique(y)) {
      predictions_i[y==i,i] <- 1  
    }
    return(predictions_i)
  }
  predictions <- adjust_predictions(obj$prediction)
  obj$conf_mat <- RSNNS::confusionMatrix(data, predictions)
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