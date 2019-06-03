loadlibrary <- function(x, repos='http://cran.fiocruz.br') 
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x, repos=repos, dep=TRUE)
    require(x)
  }
}

loadlibrary("ROCR")
loadlibrary("MLmetrics")
loadlibrary("nnet")
loadlibrary("kernlab")
loadlibrary("rattle")
loadlibrary("RSNNS")
loadlibrary("e1071")
loadlibrary("class")
loadlibrary("randomForest")
loadlibrary("Matrix")
loadlibrary("MASS")
loadlibrary("tree")

# CLASSIFICATION: General Test Class


class_prepare_train <- function(data, clabel) {
  predictors_name  = setdiff(colnames(data), clabel)
  predictors = data[,predictors_name] 
  predictand = decodeClassLabels(data[,clabel])
  regression = formula(paste(clabel, "  ~ ."))  
  return(list(predictors_name=predictors_name, predictors=predictors, predictand=predictand, regression=regression))
}

adjust_predictions <- function(predictions) {
  predictions_i = as.matrix(Matrix(rep.int(0, nrow(predictions)*ncol(predictions)), nrow=nrow(predictions), ncol=ncol(predictions)))
  y <- apply(predictions, 1, which.is.max)
  for(i in unique(y)) {
    predictions_i[y==i,i] <- 1  
  }
  return(predictions_i)
}

class_test <- function(model, test, clabel) {
  predictors_name  = setdiff(colnames(test), clabel)
  test_predictors = test[,predictors_name] 
  values = decodeClassLabels(test[,clabel])
  
  if (is.null(model$class_prediction)) {
    predictions <- predict(model$model, test_predictors, type=model$class_predtype)
  }
  else {
    predictions <- model$class_prediction(model$model, test_predictors, type=model$class_predtype)
  }
  
  predictions_i <- adjust_predictions(predictions)
  
  conf_mat = RSNNS::confusionMatrix(values, predictions_i)
  accuracy <- Accuracy(y_pred = predictions_i, y_true = values)
  f1 <- F1_Score(y_pred = predictions_i, y_true = values, positive = 1)
  sensitivity <- Sensitivity(y_pred = predictions_i, y_true = values, positive = 1)
  specificity <- Specificity(y_pred = predictions_i, y_true = values, positive = 1)
  precision <- Precision(y_pred = predictions_i, y_true = values, positive = 1)
  recall <- Recall(y_pred = predictions_i, y_true = values, positive = 1)
  metrics <- data.frame(accuracy, f1, sensitivity, specificity, specificity, recall)
  
  return (list(model = model, predictions = predictions, values = values, conf_mat = conf_mat, 
               metrics = metrics)) 
}

compute_rocr <- function(predictions, values) {
  pred <- prediction(predictions, values)
  rocr <- performance(pred, "tpr", "fpr")  
  return (rocr)
}

# CLASSIFICATION: R0

class_R0 <- function(data, clabel) {
  data[,clabel] = as.factor(data[,clabel])
  prep <- class_prepare_train(data, clabel)

  cols = apply(prep$predictand, 2, sum)
  col = match(max(cols),cols)
  model = list(cols=cols, col=col)

  model <- list(model=model, class_prediction=R0_predict, class_predtype="class")
  model$train <- class_test(model, data, clabel)
  
  return (model)   
}

R0_predict <- function(model, test, type)
{
  rows = nrow(test)
  cols = length(model$cols)
  result = Matrix(rep.int(0, rows*cols), nrow=rows, ncol=cols)
  result[,model$col] = 1  
  result = as.matrix(result)
  return(result)
}

# CLASSIFICATION: Decision Tree

class_tree <- function(data, clabel)
{
  data[,clabel] = as.factor(data[,clabel])
  prep <- class_prepare_train(data, clabel)
  
  model = tree(prep$regression, data)

  model <- list(model=model, class_prediction=NULL, class_predtype="vector")
  model$train <- class_test(model, data, clabel)
  
  return (model)   
}


# CLASSIFICATION: Random Forest

class_randomForest <- function(data, clabel, ntree = 100)
{
  data[,clabel] = as.factor(data[,clabel])
  prep <- class_prepare_train(data, clabel)
  
  model = randomForest(prep$predictors, data[,clabel], ntree=ntree)
  
  model <- list(model=model, class_prediction=NULL, class_predtype="prob")
  model$train <- class_test(model, data, clabel)
  
  return (model)   
}



# CLASSIFICATION: NaiveBayes

class_naiveBayes <- function(data, clabel)
{
  data[,clabel] = as.factor(data[,clabel])
  prep <- class_prepare_train(data, clabel)
  
  model = naiveBayes(prep$regression, data, laplace=0)

  model <- list(model=model, class_prediction=NULL, class_predtype="raw")
  model$train <- class_test(model, data, clabel)
  
  return (model)   
}

# CLASSIFICATION: MLP usando NNET

class_mlp_nnet <- function(data, clabel, neurons=3, decay=0.01, iterations=5000)
{
  data[,clabel] = as.factor(data[,clabel])
  prep <- class_prepare_train(data, clabel)

  model = nnet(prep$predictors, prep$predictand, size=neurons, decay=decay, maxit=iterations)
  
  model <- list(model=model, class_prediction=NULL, class_predtype="raw")
  model$train <- class_test(model, data, clabel)
  
  return (model)   
}

# CLASSIFICATION: MPL usando RSNNS

class_mlp_RSNNS <- function(data, clabel, neurons=3, iterations=5000)
{
  data[,clabel] = as.factor(data[,clabel])
  prep <- class_prepare_train(data, clabel)
  
  model = mlp(prep$predictors, prep$predictand, size=neurons, learnFuncParams=c(0.1), maxit=iterations)
  
  model <- list(model=model, class_prediction=NULL, class_predtype="prob")
  model$train <- class_test(model, data, clabel)
  
  return (model)     
}

# CLASSIFICATION: RBF usando RSNNS

class_rbf_RSNNS <- function(data, clabel, neurons=40, iterations=5000)
{
  data[,clabel] = as.factor(data[,clabel])
  prep <- class_prepare_train(data, clabel)
  
  model = rbfDDA(prep$predictors, prep$predictand, maxit=iterations, linOut=TRUE)
  
  model <- list(model=model, class_prediction=rbf_predict, class_predtype="prob")
  model$train <- class_test(model, data, clabel)

  return (model)     
}

rbf_predict <- function(model, test, type)
{
  result = predict(model, test, type)
  minv <- apply(result, 1, min)
  result <- result + minv
  sumv <- apply(result, 1, sum)
  result <- result / sumv
  return(result)
}

# CLASSIFICATION: SVM KERNEL RBF usando kernlab

class_svm_rbf <- function(data, clabel, C=10, sigma=0.1)
{
  data[,clabel] = as.factor(data[,clabel])
  prep <- class_prepare_train(data, clabel)

  rbf = rbfdot(sigma=sigma)
  model = ksvm(prep$regression,data=data,type="C-bsvc",kernel=rbf,C=C,prob.model=TRUE)
  
  model <- list(model=model, class_prediction=NULL, class_predtype="probabilities")
  model$train <- class_test(model, data, clabel)
  
  return (model)     
}

class_svm_poly <- function(data, clabel, C=10, degree = 1, scale = 1, offset = 1)
{
  data[,clabel] = as.factor(data[,clabel])
  prep <- class_prepare_train(data, clabel)
  
  polyk = polydot(degree = degree, scale = scale, offset = offset)
  model = ksvm(prep$regression,data=data,type="C-bsvc",kernel=polyk, C=C, prob.model=TRUE)

  model <- list(model=model, class_prediction=NULL, class_predtype="probabilities")
  model$train <- class_test(model, data, clabel)
  
  return (model)      
} 

class_svm_sigmoid <- function(data, clabel, C=10, scale = 1, offset = 1)
{
  data[,clabel] = as.factor(data[,clabel])
  prep <- class_prepare_train(data, clabel)
  
  tanhk = tanhdot(scale = scale, offset = offset)
  model = ksvm(prep$regression,data=data,type="C-bsvc",kernel=tanhk, C=C, prob.model=TRUE)
  
  model <- list(model=model, class_prediction=NULL, class_predtype="probabilities")
  model$train <- class_test(model, data, clabel)
  
  return (model)    
  
} 

# CLASSIFICATION: KNN

class_knn <- function(data, clabel, k = 3)
{
  data[,clabel] = as.factor(data[,clabel])
  prep <- class_prepare_train(data, clabel)
  
  model <- list(model=list(predictors=prep$predictors, predictand=data[,clabel], k=k), class_prediction=knn_predict, class_predtype="class")
  model$train <- class_test(model, data, clabel)
  
  return (model)    
}

knn_predict <- function(model, test, type)
{
  predictions = knn(train=model$predictors, test=test, cl=model$predictand, k=model$k, prob=TRUE)
  predictions = decodeClassLabels(predictions)  
  return(predictions)
}

