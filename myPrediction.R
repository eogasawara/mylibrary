loadlibrary <- function(x, repos='http://cran.fiocruz.br') 
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x, repos=repos, dep=TRUE)
    require(x)
  }
}

loadlibrary("nnet")
loadlibrary("kernlab")
loadlibrary("rattle")
loadlibrary("RSNNS")
loadlibrary("e1071")
loadlibrary("class")
loadlibrary("randomForest")
loadlibrary("Matrix")
loadlibrary("ROCR")
loadlibrary("MLmetrics")

# CLASSIFICATION: General Test Class
class_test <- function(model, test, clabel, predtype=NULL) {
  predictors_name  = setdiff(colnames(test), clabel)
  test_predictors = test[,predictors_name] 
  test_predictand_clabel = decodeClassLabels(test[,clabel])
  predictions = predict(model, test_predictors, type=predtype)
  classification = data.frame(predictions,test_predictand_clabel)
  confTest = RSNNS::confusionMatrix(test_predictand_clabel,predictions)
  return (list(model=model, predictions=classification, cmTest=confTest)) 
}


# CLASSIFICATION: MLP usando NNET

class_mlp_nnet <- function(data, clabel, neurons=3, decay=0.01, iterations=5000)
{
  predictors_name  = setdiff(colnames(data), clabel)
  data_predictors = data[,predictors_name] 
  data_predictand_clabel = decodeClassLabels(data[,clabel])
  data[,clabel] = as.factor(data[,clabel])
  class_formula = formula(paste(clabel, "  ~ ."))  

  model = nnet(data_predictors, data_predictand_clabel, size=neurons, decay=decay, maxit=iterations)
  predtype = "raw"
  predictions = predict(model, data_predictors, type=predtype)  

  classification = data.frame(predictions,data_predictand_clabel)
  confTrain = RSNNS::confusionMatrix(data_predictand_clabel,predictions)
  return (list(model=model, predtype=predtype, predictions=classification, cmTrain=confTrain)) 
}

# CLASSIFICATION: SVM KERNEL RBF usando kernlab

class_svm_rbf <- function(data, clabel, C=10, sigma=0.1)
{
  predictors_name  = setdiff(colnames(data), clabel)
  data_predictors = data[,predictors_name] 
  data_predictand_clabel = decodeClassLabels(data[,clabel])
  data[,clabel] = as.factor(data[,clabel])
  class_formula = formula(paste(clabel, "  ~ ."))  

  rbf = rbfdot(sigma=sigma)
  model = ksvm(class_formula,data=data,type="C-bsvc",kernel=rbf,C=C,prob.model=TRUE)
  predtype = "probabilities"
  predictions = predict(model, data_predictors, type=predtype)  

  classification = data.frame(predictions,data_predictand_clabel)
  confTrain = RSNNS::confusionMatrix(data_predictand_clabel,predictions)
  return (list(model=model, predtype=predtype, predictions=classification, cmTrain=confTrain)) 
}

class_svm_poly <- function(data, clabel, C=10, degree = 1, scale = 1, offset = 1)
{
  predictors_name  = setdiff(colnames(data), clabel)
  data_predictors = data[,predictors_name] 
  data_predictand_clabel = decodeClassLabels(data[,clabel])
  data[,clabel] = as.factor(data[,clabel])
  class_formula = formula(paste(clabel, "  ~ ."))  
  
  polyk = polydot(degree = degree, scale = scale, offset = offset)
  model = ksvm(class_formula,data=data,type="C-bsvc",kernel=polyk, C=C, prob.model=TRUE)
  predtype = "probabilities"
  predictions = predict(model, data_predictors, type=predtype)  
  
  classification = data.frame(predictions,data_predictand_clabel)
  confTrain = RSNNS::confusionMatrix(data_predictand_clabel,predictions)
  return (list(model=model, predtype=predtype, predictions=classification, cmTrain=confTrain)) 
} 

class_svm_sigmoid <- function(data, clabel, C=10, scale = 1, offset = 1)
{
  predictors_name  = setdiff(colnames(data), clabel)
  data_predictors = data[,predictors_name] 
  data_predictand_clabel = decodeClassLabels(data[,clabel])
  data[,clabel] = as.factor(data[,clabel])
  class_formula = formula(paste(clabel, "  ~ ."))  
  
  tanhk = tanhdot(scale = scale, offset = offset)
  model = ksvm(class_formula,data=data,type="C-bsvc",kernel=tanhk, C=C, prob.model=TRUE)
  predtype = "probabilities"
  predictions = predict(model, data_predictors, type=predtype)  
  
  classification = data.frame(predictions,data_predictand_clabel)
  confTrain = RSNNS::confusionMatrix(data_predictand_clabel,predictions)
  return (list(model=model, predtype=predtype, predictions=classification, cmTrain=confTrain)) 
} 

# CLASSIFICATION: MPL usando RSNNS

class_mlp_RSNNS <- function(data, clabel, neurons=3, iterations=5000)
{
  predictors_name  = setdiff(colnames(data), clabel)
  data_predictors = data[,predictors_name] 
  data_predictand_clabel = decodeClassLabels(data[,clabel])
  data[,clabel] = as.factor(data[,clabel])
  class_formula = formula(paste(clabel, "  ~ ."))  
  
  model = mlp(data_predictors, data_predictand_clabel, size=neurons, learnFuncParams=c(0.1), maxit=iterations)
  predtype = NULL
  predictions = predict(model, data_predictors)  
  
  classification = data.frame(predictions,data_predictand_clabel)
  confTrain = RSNNS::confusionMatrix(data_predictand_clabel,predictions)
  return (list(model=model, predtype=predtype, predictions=classification, cmTrain=confTrain)) 
}

# CLASSIFICATION: RBF usando RSNNSz

class_rbf_RSNNS <- function(data, clabel, neurons=40, iterations=5000)
{
  predictors_name  = setdiff(colnames(data), clabel)
  data_predictors = data[,predictors_name] 
  data_predictand_clabel = decodeClassLabels(data[,clabel])
  data[,clabel] = as.factor(data[,clabel])
  class_formula = formula(paste(clabel, "  ~ ."))  
  
  model = rbf(data_predictors, data_predictand_clabel, size=neurons, maxit=iterations,
              initFuncParams=c(0, 1, 0, 0.01, 0.01),
              learnFuncParams=c(1e-8, 0, 1e-8, 0.1, 0.8), linOut=TRUE)  
  predtype = NULL
  predictions = predict(model, data_predictors)  
  
  classification = data.frame(predictions,data_predictand_clabel)
  confTrain = RSNNS::confusionMatrix(data_predictand_clabel,predictions)
  return (list(model=model, predtype=predtype, predictions=classification, cmTrain=confTrain)) 
}

# CLASSIFICATION: NaiveBayes

class_naiveBayes <- function(data, clabel)
{
  predictors_name  = setdiff(colnames(data), clabel)
  data_predictors = data[,predictors_name] 
  data_predictand_clabel = decodeClassLabels(data[,clabel])
  data[,clabel] = as.factor(data[,clabel])
  class_formula = formula(paste(clabel, "  ~ ."))  
  
  model = naiveBayes(class_formula, data, laplace=0)
  predtype = "raw"
  predictions = predict(model, data_predictors, type=predtype)  
  
  classification = data.frame(predictions,data_predictand_clabel)
  confTrain = RSNNS::confusionMatrix(data_predictand_clabel,predictions)
  return (list(model=model, predtype=predtype, predictions=classification, cmTrain=confTrain)) 
}

# CLASSIFICATION: Random Forest

class_randomForest <- function(data, clabel, ntree = 100)
{
  predictors_name  = setdiff(colnames(data), clabel)
  data_predictors = data[,predictors_name] 
  data_predictand_clabel = decodeClassLabels(data[,clabel])
  data[,clabel] = as.factor(data[,clabel])
  class_formula = formula(paste(clabel, "  ~ ."))  
  
  model = randomForest(data_predictors, data[,clabel], ntree=ntree)
  predtype = "prob"
  predictions = predict(model, data_predictors, type=predtype)  
  
  classification = data.frame(predictions,data_predictand_clabel)
  confTrain = RSNNS::confusionMatrix(data_predictand_clabel,predictions)
  return (list(model=model, predtype=predtype, predictions=classification, cmTrain=confTrain))   
}

# CLASSIFICATION: KNN

knn_predict <- function(data, test, clabel, k = 10)
{
  predictors_name  = setdiff(colnames(data), clabel)
  
  data_clabel = as.factor(data[,clabel])
  data_predictors = data[,predictors_name] 
  data_predictand_clabel = decodeClassLabels(data_clabel)
  
  test_clabel =  as.factor(test[,clabel])
  test_predictors = test[,predictors_name] 
  test_predictand_clabel = decodeClassLabels(test_clabel)
  
  predictions_train = knn(data_predictors, data_predictors, data_clabel, k=k, prob=TRUE)
  predictions = knn(data_predictors, test_predictors, data_clabel, k=k, prob=TRUE)
  
  predictions_train = decodeClassLabels(predictions_train)
  predictions = decodeClassLabels(predictions)
  
  classification = data.frame(predictions, test_predictand_clabel)
  confTrain = RSNNS::confusionMatrix(data_predictand_clabel, predictions_train)
  confTest = RSNNS::confusionMatrix(test_predictand_clabel, predictions)
  return (list(predictions=classification, cmTrain=confTrain, cmTest=confTest))  
}

# CLASSIFICATION: R0

R0_predict <- function(data, test, clabel)
{
  predictors_name  = setdiff(colnames(data), clabel)
  
  data_predictors = data[,predictors_name] 
  data_predictand_clabel = decodeClassLabels(data[,clabel])
  
  test_predictors = test[,predictors_name] 
  test_predictand_clabel = decodeClassLabels(test[,clabel])
  
  maxclabel = apply(data_predictand_clabel, 2, sum)
  maxclabel_val = max(maxclabel)
  col = match(maxclabel_val,maxclabel)
  
  r0_matrix <- function(matrix, col) {
    rows = nrow(matrix)
    cols = ncol(matrix)
    result = Matrix(rep.int(0, rows*cols), nrow=rows, ncol=cols)
    result[,col] = 1  
    result = as.matrix(result)
    return(result)
  }
  
  predictions_train = r0_matrix(data_predictand_clabel, col)
  predictions = r0_matrix(test_predictand_clabel, col)
  
  classification = data.frame(predictions,test_predictand_clabel)
  confTrain = RSNNS::confusionMatrix(data_predictand_clabel,predictions_train)
  confTest = RSNNS::confusionMatrix(test_predictand_clabel,predictions)
  return (list(predictions=classification, cmTrain=confTrain, cmTest=confTest))  
}
