# version 1.0
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myClassificationEvaluation.R")
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myFitting.R")

#loadlibrary("leaps")
#loadlibrary("doBy")

# feature selection
feature_selection <- function(data, attribute) {
  data[,attribute] = as.factor(data[,attribute])
  obj <- rel_transform(data)
  obj$attribute <- attribute
  class(obj) <- append("feature_selection", class(obj))    
  return(obj)
}

action.feature_selection <- function(obj) {
  data = obj$data[,c(obj$features, obj$attribute)]
  return(data)
}

#Lasso
feature_selection_lasso <- function(data, attribute) {
  obj <- feature_selection(data, attribute)
  class(obj) <- append("feature_selection_lasso", class(obj))    
  return(obj)
}

prepare.feature_selection_lasso <- function(obj) {
  loadlibrary("glmnet")
  data = data.frame(obj$data)
  if (!is.numeric(data[,obj$attribute]))
    data[,obj$attribute] =  as.numeric(data[,obj$attribute])
  nums = unlist(lapply(data, is.numeric))
  data = data[ , nums]
  
  predictors_name  = setdiff(colnames(data), obj$attribute)
  predictors = as.matrix(data[,predictors_name])
  predictand = data[,obj$attribute]
  grid = 10^seq(10, -2, length = 100)
  cv.out = cv.glmnet(predictors, predictand, alpha = 1)
  bestlam = cv.out$lambda.min
  out = glmnet(predictors, predictand, alpha = 1, lambda = grid)
  lasso.coef = predict(out,type = "coefficients", s = bestlam)
  l = lasso.coef[(lasso.coef[,1]) != 0,0]
  vec = rownames(l)[-1]
  
  obj$features <- vec

  return(obj)
}

# forward stepwise selection

feature_selection_fss <- function(data, attribute) {
  obj <- feature_selection(data, attribute)
  class(obj) <- append("feature_selection_fss", class(obj))    
  return(obj)
}

prepare.feature_selection_fss <- function(obj) {
  loadlibrary("leaps")  
  data = data.frame(obj$data)
  if (!is.numeric(data[,obj$attribute]))
    data[,obj$attribute] =  as.numeric(data[,obj$attribute])
  nums = unlist(lapply(data, is.numeric))
  data = data[ , nums]
  
  predictors_name  = setdiff(colnames(data), obj$attribute)
  predictors = as.matrix(data[,predictors_name])
  predictand = data[,obj$attribute]
  
  regfit.fwd = regsubsets(predictors, predictand, nvmax=ncol(data)-1, method="forward")  
  summary(regfit.fwd)
  reg.summaryfwd = summary(regfit.fwd)
  b1 = which.max(reg.summaryfwd$adjr2)
  t = coef(regfit.fwd,b1)
  vec = names(t)[-1]

  obj$features <- vec
  
  return(obj)
}

# information gain

feature_selection_ig <- function(data, attribute) {
  obj <- feature_selection(data, attribute)
  class(obj) <- append("feature_selection_ig", class(obj))    
  return(obj)
}

prepare.feature_selection_ig <- function(obj) {
  loadlibrary("FSelector")
  data <- data.frame(obj$data)
  
  class_formula <- formula(paste(obj$attribute, "  ~ ."))
  weights <- information.gain(class_formula, data)
  
  tab <- data.frame(weights)
  tab <- orderBy(~-attr_importance, data=tab)
  tab$i <- row(tab)
  tab$import_acum <- cumsum(tab$attr_importance)
  myfit <- curvature_min(tab$import_acum)
  myfit <- prepare(myfit)  
  tab <- tab[tab$import_acum <= myfit$y, ]
  vec <- rownames(tab)
  
  obj$features <- vec
  
  return(obj)
}

# relief

feature_selection_relief <- function(data, attribute) {
  obj <- feature_selection(data, attribute)
  class(obj) <- append("feature_selection_relief", class(obj))    
  return(obj)
}

prepare.feature_selection_relief <- function(obj) {
  loadlibrary("doBy")
  
  data <- data.frame(obj$data)
  
  if (!is.numeric(data[,obj$attribute]))
    data[,obj$attribute] =  as.numeric(data[,obj$attribute])
  nums = unlist(lapply(data, is.numeric))
  data = data[ , nums]
  
  class_formula = formula(paste(obj$attribute, "  ~ ."))
  weights = relief(class_formula, data)
  
  tab=data.frame(weights)
  tab=orderBy(~-attr_importance, data=tab)
  tab$i=row(tab)
  tab$import_acum=cumsum(tab$attr_importance)
  myfit <- curvature_min(tab$import_acum)
  myfit <- prepare(myfit)  
  tab <- tab[tab$import_acum <= myfit$y, ]
  vec = rownames(tab)
  
  obj$features <- vec
  
  return(obj)
}

