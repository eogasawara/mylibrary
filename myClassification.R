# version 1.1
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
  obj$x <- setdiff(colnames(data), obj$attribute)  
  return(obj)
}

# zero_rule
classification_majority <- function(attribute, slevels=NULL) {
  obj <- classification(attribute, slevels)
  
  class(obj) <- append("classification_majority", class(obj))    
  return(obj)
}

prepare.classification_majority <- function(obj, data) {
  data <- adjust.data.frame(data)
  data[,obj$attribute] <- adjust.factor(data[,obj$attribute], obj$ilevels, obj$slevels)
  obj <- prepare.classification(obj, data)
  
  loadlibrary("RSNNS")
  y <- decodeClassLabels(data[,obj$attribute])
  cols <- apply(y, 2, sum)
  col <- match(max(cols),cols)
  obj$model <- list(cols=cols, col=col)
  
  obj <- register_log(obj)  
  return(obj)
}

action.classification_majority <- function(obj, x) {
  loadlibrary("Matrix")  
  rows <- nrow(x)
  cols <- length(obj$model$cols)
  prediction <- Matrix(rep.int(0, rows*cols), nrow=rows, ncol=cols)
  prediction[,obj$model$col] <- 1
  colnames(prediction) <- names(obj$model$cols)
  prediction <- as.matrix(prediction)
  return(prediction)
}

# decision_tree
classification_dtree <- function(attribute, slevels=NULL) {
  obj <- classification(attribute, slevels)
  
  class(obj) <- append("classification_dtree", class(obj))    
  return(obj)
}

prepare.classification_dtree <- function(obj, data) {
  data <- adjust.data.frame(data)
  data[,obj$attribute] <- adjust.factor(data[,obj$attribute], obj$ilevels, obj$slevels)
  obj <- prepare.classification(obj, data)
  loadlibrary("tree")
  
  regression <- formula(paste(obj$attribute, "  ~ ."))  
  obj$model <- tree(regression, data)
  
  obj <- register_log(obj)  
  return(obj)
}

action.classification_dtree <- function(obj, x) {
  x <- adjust.data.frame(x)
  x <- x[,obj$x]   
  prediction <- predict(obj$model, x, type="vector")  
  return(prediction)
}

# naive_bayes
classification_nb <- function(attribute, slevels=NULL) {
  obj <- classification(attribute, slevels)
  
  class(obj) <- append("classification_nb", class(obj))    
  return(obj)
}

prepare.classification_nb <- function(obj, data) {
  data <- adjust.data.frame(data)
  data[,obj$attribute] <- adjust.factor(data[,obj$attribute], obj$ilevels, obj$slevels)
  obj <- prepare.classification(obj, data)
  loadlibrary("e1071")
  
  regression <- formula(paste(obj$attribute, "  ~ ."))  
  obj$model <- naiveBayes(regression, data, laplace=0)
  
  obj <- register_log(obj)
  return(obj)
}

action.classification_nb  <- function(obj, x) {
  x <- adjust.data.frame(x)
  x <- x[,obj$x]
  prediction <- predict(obj$model, x, type="raw")
  return(prediction)
}

# random_forest
classification_rf <- function(attribute, slevels=NULL, mtry = NULL, ntree = seq(5, 50, 5)) {
  obj <- classification(attribute, slevels)
  
  obj$ntree <- ntree
  obj$mtry <- mtry
  
  class(obj) <- append("classification_rf", class(obj))
  return(obj)
}

prepare.classification_rf <- function(obj, data) {
  data <- adjust.data.frame(data)
  data[,obj$attribute] <- adjust.factor(data[,obj$attribute], obj$ilevels, obj$slevels)
  obj <- prepare.classification(obj, data)
  loadlibrary("randomForest")
  
  if (is.null(obj$mtry))
    obj$mtry <- ceiling(c(1,1.5,2)*sqrt(ncol(data)))
  
  x <- data[,obj$x]
  y <- data[,obj$attribute]
  
  ranges <- list(mtry=obj$mtry, ntree=obj$ntree)
  obj$model <- tune.classification(x = x, y = y, ranges = ranges, train.func = randomForest, pred.fun = predict.classification_rf)

  params <- attr(obj$model, "params") 
  msg <- sprintf("mtry=%d,ntree=%d", params$mtry, params$ntree)
  obj <- register_log(obj, msg)
  return(obj)
}

action.classification_rf  <- function(obj, x) {
  x <- adjust.data.frame(x)
  x <- x[,obj$x]   
  prediction <- predict.classification_rf(obj$model, x)  
  return(prediction)
}

predict.classification_rf <- function(model, x) {
  prediction <- predict(model, x, type="prob")  
  return(prediction)
}

# mlp_nnet
classification_mlp <- function(attribute, slevels=NULL, size=NULL, decay=seq(0, 1, 0.0335), maxit=1000) {
  obj <- classification(attribute, slevels)
  obj$maxit <- maxit
  obj$size <- size
  obj$decay <- decay
  
  class(obj) <- append("classification_mlp", class(obj))    
  return(obj)
}

prepare.classification_mlp <- function(obj, data) {
  data <- adjust.data.frame(data)
  data[,obj$attribute] <- adjust.factor(data[,obj$attribute], obj$ilevels, obj$slevels)
  obj <- prepare.classification(obj, data)
  loadlibrary("nnet")
  
  if (is.null(obj$size))
    obj$size <- ceiling(sqrt(ncol(data)))

  x <- data[,obj$x]
  y <- data[,obj$attribute]
  
  ranges <- list(maxit=obj$maxit, decay = obj$decay, size=obj$size)
  obj$model <- tune.classification(x = x, y = y, ranges = ranges, train.func = train.classification_mlp, pred.fun = predict.classification_mlp)
  
  params <- attr(obj$model, "params") 
  msg <- sprintf("size=%d,decay=%.2f", params$size, params$decay)
  obj <- register_log(obj, msg)
  return(obj)
}

train.classification_mlp <- function (x, y, size, decay, maxit) {
  return (nnet(x,decodeClassLabels(y),size=size,decay=decay,maxit=maxit,trace=FALSE))
}

predict.classification_mlp <- function(model, x) {
  prediction <- predict(model, x, type="raw")  
  return(prediction)
}

action.classification_mlp  <- function(obj, x) {
  x <- adjust.data.frame(x)
  x <- x[,obj$x]   
  prediction <- predict.classification_mlp(obj$model, x)  
  return(prediction)
}


# classification_svm 
classification_svm <- function(attribute, slevels=NULL, epsilon=seq(0,1,0.2), cost=seq(20,100,20), kernel="radial") {
  #kernel: linear, radial, polynomial, sigmoid
  #studio: https://rpubs.com/Kushan/296706  
  obj <- classification(attribute, slevels)
  obj$kernel <- kernel
  obj$epsilon <- epsilon
  obj$cost <- cost
  
  class(obj) <- append("classification_svm", class(obj))    
  return(obj)
}

prepare.classification_svm <- function(obj, data) {
  data <- adjust.data.frame(data)
  data[,obj$attribute] <- adjust.factor(data[,obj$attribute], obj$ilevels, obj$slevels)
  obj <- prepare.classification(obj, data)
  loadlibrary("e1071")
  
  x <- data[,obj$x]
  y <- data[,obj$attribute]
  
  ranges <- list(epsilon=obj$epsilon, cost=obj$cost, kernel=obj$kernel)
  obj$model <- tune.classification(x = x, y = y, ranges = ranges, train.func = train.classification_svm, pred.fun = predict.classification_svm)
  
  params <- attr(obj$model, "params") 
  msg <- sprintf("epsilon=%.1f,cost=%.3f", params$epsilon, params$cost)
  obj <- register_log(obj, msg)
  return(obj)
}

train.classification_svm <- function (x, y, epsilon, cost, kernel) {
  model <- svm(x, y, probability=TRUE, epsilon=epsilon, cost=cost, kernel=kernel) 
  attr(model, "slevels")  <- levels(y)
  return (model)
}

predict.classification_svm <- function(model, x) {
  prediction <- predict(model, x, probability = TRUE) 
  prediction <- attr(prediction, "probabilities")
  slevels <- attr(model, "slevels")
  prediction <- prediction[,slevels]
  return(prediction)
}

action.classification_svm  <- function(obj, x) {
  x <- adjust.data.frame(x)
  x <- x[,obj$x]   
  prediction <- predict.classification_svm(obj$model, x)
  return(prediction)
}

# classification_knn 
classification_knn <- function(attribute, slevels=NULL, k=1:30) {
  obj <- classification(attribute, slevels)
  obj$k <- k
  class(obj) <- append("classification_knn", class(obj))    
  return(obj)
}

prepare.classification_knn <- function(obj, data) {
  data <- adjust.data.frame(data)
  data[,obj$attribute] <- adjust.factor(data[,obj$attribute], obj$ilevels, obj$slevels)
  obj <- prepare.classification(obj, data)
  
  loadlibrary("class")
  
  x <- data[,obj$x]
  y <- data[,obj$attribute]
  
  ranges <- list(k = obj$k, stub = 0)
  obj$model <- tune.classification(x = x, y = y, ranges = ranges, train.func = train.classification_knn, pred.fun = predict.classification_knn)

  params <- attr(obj$model, "params") 
  msg <- sprintf("k=%d", params$k)
  obj <- register_log(obj, msg)
  return(obj)
}

train.classification_knn <- function (x, y, k, ...) {
  model <- list(x=x, y=y, k=k)
  return (model)
}

predict.classification_knn <- function(model, x) {
  loadlibrary("class")
  prediction <- knn(train=model$x, test=x, cl=model$y, prob=TRUE)
  prediction <- decodeClassLabels(prediction)  
  return(prediction)
}

action.classification_knn  <- function(obj, x) {
  x <- adjust.data.frame(x)
  x <- x[,obj$x] 
  prediction <- predict.classification_knn(obj$model, x)
  return(prediction)
}

# classification_cnn 
classification_cnn <- function(attribute, slevels=NULL, neurons=c(3,5,10,16,32), epochs = c(100, 150, 200)) {
  obj <- classification(attribute, slevels)
  obj$neurons <- neurons
  obj$epochs <- epochs
  
  class(obj) <- append("classification_cnn", class(obj))    
  return(obj)
}

prepare.classification_cnn <- function(obj, data) {
  data <- adjust.data.frame(data)
  data[,obj$attribute] <- adjust.factor(data[,obj$attribute], obj$ilevels, obj$slevels)
  obj <- prepare.classification(obj, data)
  loadlibrary("dplyr")
  loadlibrary("tfdatasets")
  loadlibrary("tensorflow")
  loadlibrary("keras")  
  
  x <- data[,obj$x]
  y <- data[,obj$attribute]
  
  tf$get_logger()$setLevel('ERROR')
  ranges <- list(neurons = obj$neurons, epochs = obj$epochs)
  obj$model <- tune.classification(x = x, y = y, ranges = ranges, train.func = train.classification_cnn)
  tf$get_logger()$setLevel('WARNING')
  
  params <- attr(obj$model, "params") 
  msg <- sprintf("neurons=%d,epochs=%d", params$neurons, params$epochs)
  obj <- register_log(obj, msg)
  return(obj)
}

train.classification_cnn <- function(x, y, neurons, epochs, ...) {
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

action.classification_cnn  <- function(obj, x) {
  x <- adjust.data.frame(x)
  x <- x[,obj$x] 
  prediction <- predict(obj$model, x)
  colnames(prediction) <- obj$slevels
  return(prediction)
}


classification.reproduce <- TRUE
classification.check_reproduce <- function() {
  if (classification.reproduce)
    set.seed(1)
}

tune.classification <- function (x, y, ranges, folds=3, train.func, pred.fun = predict) {
  ranges <- expand.grid(ranges)
  n <- nrow(ranges)
  accuracies <- rep(0,n)
  
  data <- data.frame(i = 1:nrow(x), idx = 1:nrow(x))
  folds <- k_fold(sample_random(), data, folds)
  
  i <- 1
  if (n > 1) {
    for (i in 1:n) {
      for (j in 1:length(folds)) {
        classification.check_reproduce()
        tt <- train_test_from_folds(folds, j)
        
        params <- append(list(x = x[tt$train$i,], y = y[tt$train$i]), as.list(ranges[i,]))
        model <- do.call(train.func, params)
        prediction <- pred.fun(model, x[tt$test$i,]) 
        accuracies[i] <- accuracies[i] + evaluation.classification(decodeClassLabels(y[tt$test$i]), prediction)$accuracy
      }
    }
    i <- which.max(accuracies)
  }
  classification.check_reproduce()
  params <- append(list(x = x, y = y), as.list(ranges[i,]))
  model <- do.call(train.func, params)
  attr(model, "params") <- as.list(ranges[i,])
  return(model)
}



#evaluation.classification

evaluation.classification <- function(data, prediction) {
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
  
  attr(obj, "class") <- "evaluation.classification"  
  return(obj)
}

roc_curve <- function(obj) {
  UseMethod("roc_curve")
}

roc_curve.evaluation.classification <- function(obj) {
  loadlibrary("ROCR")
  pred <- prediction(obj$prediction, obj$data)
  rocr <- performance(pred, "tpr", "fpr")  
  return (rocr)  
}