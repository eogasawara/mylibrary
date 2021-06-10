# version 1.0
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myTransform.R")
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/mySample.R")

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
  
  predictors <- data[,obj$predictors]
  predictand <- data[,obj$attribute]
  tuned <- tune.randomForest(x=predictors, y=predictand, mtry=obj$mtry, ntree=obj$ntree)
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
class_mlp <- function(attribute, slevels=NULL, neurons=NULL, decay=seq(0, 1, 0.1), maxit=1000) {
  obj <- classification(attribute, slevels)
  obj$maxit <- maxit
  if (is.null(neurons))
    neurons <- ceiling(sqrt(ncol(data)))
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
  tuned <- tune.nnet(regression, data=data, trace=FALSE, maxit=obj$maxit, decay = obj$decay, size=obj$neurons)
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
class_svm <- function(attribute, slevels=NULL, epsilon=seq(0.5,1,0.5), cost=seq(20,100,20), kernel="radial") {
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
class_knn <- function(attribute, slevels=NULL, k=1:10) {
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
class_cnn <- function(attribute, slevels=NULL, neurons=c(2,3,4,5,8,10,16,32,64,128), epochs = 100) {
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
  
  predictors <- data[,obj$predictors]
  predictand <- data[,obj$attribute]
  
  obj$model <- tune.class_cnn(x=predictors, y=predictand, neurons = obj$neurons, epochs = obj$epochs)  
  obj <- register_log(obj)
  return(obj)
}

action.class_cnn  <- function(obj, data) {
  data <- adjust.data.frame(data)
  predictors <- data[,obj$predictors] 
  prediction <- predict(obj$model, predictors)
  colnames(prediction) <- obj$slevels
  return(prediction)
}

#functions created from tune

train.class_cnn <- function(x, y, neurons, epochs, ...) {
  data <- adjust.data.frame(x)
  yhot <- decodeClassLabels(y)
  
  spec <- feature_spec(cbind(data, y), y ~ . ) %>% 
    step_numeric_column(all_numeric(), normalizer_fn = scaler_standard()) %>% 
    fit()
  
  input <- layer_input_from_dataset(data)
  
  output <- input %>% 
    layer_dense_features(dense_features(spec)) %>% 
    layer_dense(units = neurons, activation = "relu") %>% 
    layer_dense(units = neurons, activation = "relu") %>% 
    layer_dense(units = ncol(yhot), activation = "sigmoid")
  
  model <- keras_model(input, output)
  
  model %>% compile(
    loss = 'categorical_crossentropy',
    optimizer = optimizer_rmsprop(), #optimizer_sgd(lr = 0.01),
    metrics = 'accuracy'
  )
  
  history <- model %>% 
    fit(
      x = data,
      y = yhot, 
      epochs = epochs, 
      validation_split = 0.2,
      verbose = 0
    )
  return(model)
}

tune.general.classification <- function (x, y, ranges, folds=3, train.func, pred.fun = predict) {
  ranges <- expand.grid(ranges)
  n <- nrow(ranges)
  accuracies <- rep(0,n)
  data <- adjust.data.frame(cbind(x, y))
  folds <- k_fold(sample_random(), data, folds)
  
  i <- 1
  if (n > 1) {
    for (i in 1:n) {
      for (j in 1:length(folds)) {
        tt <- train_test_from_folds(folds, j)
        params <- append(list(x = tt$train[colnames(x)], y = tt$train$y), as.list(ranges[i,]))
        model <- do.call(train.func, params)
        prediction <- predict(model, tt$test[colnames(x)]) 
        value <- decodeClassLabels(tt$test$y)
        accuracies[i] <- accuracies[i] + classif_evaluation(value, prediction)$accuracy 
      }
    }
    i <- which.max(accuracies)
  }
  params <- append(list(x = x, y = y), as.list(ranges[i,]))
  model <- do.call(train.func, params)
  attr(model, "params") <- as.list(ranges[i,])
  return(model)
}

tune.class_cnn <- function (x, y, neurons, epochs) {
  tf$get_logger()$setLevel('ERROR')
  ranges <- list(neurons = neurons, epochs = epochs)
  model <- tune.general.classification(x = x, y = y, ranges = ranges, train.func = train.class_cnn)
  tf$get_logger()$setLevel('WARNING')
  return(model)
}



#classif_evaluation

classif_evaluation <- function(data, prediction) {
  obj <- list(data=data, prediction=prediction)
  
  loadlibrary("RSNNS")  
  loadlibrary("nnet")  
  loadlibrary("MLmetrics")  
  adjust_predictions <- function(predictions) {
    predictions_i <- matrix(rep.int(0, nrow(predictions)*ncol(predictions)), nrow=nrow(predictions), ncol=ncol(predictions))
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