# version 1.0
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myClassificationEvaluation.R")

# classif

classification <- function(data, attribute) {
  data[,attribute] = as.factor(data[,attribute])
  obj <- rel_transform(data)
  obj$attribute <- attribute
  obj$predictors <- setdiff(colnames(obj$data), attribute)  
  class(obj) <- append("classification", class(obj))    
  return(obj)
}

# zero_rule

classif_zero_rule <- function(data, attribute) {
  obj <- classification(data, attribute)
  class(obj) <- append("classif_zero_rule", class(obj))    
  return(obj)
}

prepare.classif_zero_rule <- function(obj) {
  loadlibrary("RSNNS")
  predictand = decodeClassLabels(obj$data[,obj$attribute])

  cols <- apply(predictand, 2, sum)
  col <- match(max(cols),cols)
  obj$model <- list(cols=cols, col=col)

  return(obj)
}

action.classif_zero_rule <- function(obj) {
  loadlibrary("Matrix")  
  rows <- nrow(obj$data)
  cols <- length(obj$model$cols)
  prediction <- Matrix(rep.int(0, rows*cols), nrow=rows, ncol=cols)
  prediction[,obj$model$col] <- 1
  colnames(prediction) <- names(obj$model$cols)
  prediction <- as.matrix(prediction)
  return(prediction)
}


# decision_tree

classif_decision_tree <- function(data, attribute) {
  obj <- classification(data, attribute)
  class(obj) <- append("classif_decision_tree", class(obj))    
  return(obj)
}

prepare.classif_decision_tree <- function(obj) {
  loadlibrary("tree")
  regression <- formula(paste(obj$attribute, "  ~ ."))  
  
  obj$model <- tree(regression, obj$data)
  
  return(obj)
}

action.classif_decision_tree <- function(obj) {
  predictors = obj$data[,obj$predictors]   
  prediction <- predict(obj$model, predictors, type="vector")  
  return(prediction)
}



# random_forest

classif_random_forest <- function(data, attribute, ntree = 100) {
  obj <- classification(data, attribute)
  obj$ntree <- ntree
  class(obj) <- append("classif_random_forest", class(obj))    
  return(obj)
}

prepare.classif_random_forest <- function(obj) {
  predictors = obj$data[,obj$predictors] 
  predictand = obj$data[,obj$attribute]
  
  loadlibrary("randomForest")
  obj$model <- randomForest(predictors, predictand, ntree=obj$ntree)

  return(obj)
}

action.classif_random_forest  <- function(obj) {
  predictors = obj$data[,obj$predictors]   
  prediction <- predict(obj$model, predictors, type="prob")  
  return(prediction)
}
