# version 1.0
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myClassificationEvaluation.R")

# classification

classification <- function(data, attribute) {
  data[,attribute] = as.factor(data[,attribute])
  obj <- rel_transform(data)
  obj$attribute <- attribute
  obj$predictors <- setdiff(colnames(obj$data), attribute)  
  class(obj) <- append("classification", class(obj))    
  return(obj)
}

classification_zerorule <- function(data, attribute) {
  obj <- classification(data, attribute)
  class(obj) <- append("classification_zerorule", class(obj))    
  return(obj)
}


prepare.classification_zerorule <- function(obj) {
  loadlibrary("RSNNS")
  #  predictors = obj$data[,obj$predictors] 
  predictand = decodeClassLabels(obj$data[,obj$attribute])
#  regression = formula(paste(obj$attribute, "  ~ ."))  
  
  cols = apply(predictand, 2, sum)
  col = match(max(cols),cols)
  model = list(cols=cols, col=col)
  #model <- list(model=model, class_prediction=ZeroRule_predict, class_predtype="class")  
  obj$model <- model

  return(obj)
}

action.classification_zerorule <- function(obj) {
  loadlibrary("Matrix")  
  rows <- nrow(obj$data)
  cols <- length(obj$model$cols)
  result <- Matrix(rep.int(0, rows*cols), nrow=rows, ncol=cols)
  result[,obj$model$col] <- 1
  colnames(result) <- names(obj$model$cols)
  result <- as.matrix(result)
  return(result)
}
