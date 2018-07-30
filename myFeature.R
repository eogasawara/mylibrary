loadlibrary <- function(x, repos='http://cran.fiocruz.br') 
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x, repos=repos, dep=TRUE)
    require(x)
  }
}

loadlibrary("glmnet")
loadlibrary("leaps")
loadlibrary("FSelector")
loadlibrary("doBy")



# FEATURE SELECTION


#Lasso

fs.lasso <- function(data, class)
{
  data = data.frame(data)
  dataorg <- data
  if (!is.numeric(data[,class]))
    data[,class] =  as.numeric(data[,class])
  nums = unlist(lapply(data, is.numeric))
  data = data[ , nums]
  predictors_name  = setdiff(colnames(data), class)
  predictors = as.matrix(data[,predictors_name])
  predictand = data[,class]
  grid = 10^ seq (10,-2, length = 100)
  cv.out = cv.glmnet (predictors, predictand, alpha = 1)
  bestlam = cv.out$lambda.min
  out = glmnet(predictors, predictand, alpha = 1, lambda = grid)
  lasso.coef = predict (out,type = "coefficients", s = bestlam)
  l = lasso.coef[(lasso.coef[,1]) != 0,0]
  vec = rownames(l)[-1]
  data = dataorg[,c(vec, class)]
  return (list(data=data, features=vec))
}


# Forward Stepwise Selection

fs.fss <- function(data, class)
{
  data = data.frame(data)
  dataorg <- data
  if (!is.numeric(data[,class]))
    data[,class] =  as.numeric(data[,class])
  nums = unlist(lapply(data, is.numeric))
  data = data[ , nums]
  
  predictors_name  = setdiff(colnames(data), class)
  predictors = as.matrix(data[,predictors_name])
  predictand = data[,class]
  
  regfit.fwd = regsubsets(predictors, predictand, nvmax=ncol(data)-1, method="forward")  
  summary(regfit.fwd)
  reg.summaryfwd = summary(regfit.fwd)
  b1 = which.max(reg.summaryfwd$adjr2)
  t = coef(regfit.fwd,b1)
  vec = names(t)[-1]
  data = dataorg[,c(vec, class)]
  return (list(data=data, features=vec))
}

# Correlation-based Feature Selection (CFS)

fs.cfs <- function(data, class)
{
  class_formula = formula(paste(class, "  ~ ."))
  vec = cfs(class_formula, data)
  data = data[,c(vec, class)]
  return (list(data=data, features=vec))
}

# Information Gain

fs.ig <- function(data, class)
{
  class_formula = formula(paste(class, "  ~ ."))
  weights = information.gain(class_formula, data)
  
  tab=data.frame(weights)
  tab=orderBy(~-attr_importance, data=tab)
  tab$i=row(tab)
  tab$import_acum=cumsum(tab$attr_importance)
  res = curvature.min(tab$i, tab$import_acum)
  tab = tab[tab$import_acum <= res$y,]
  vec = rownames(tab)
  data = data[,c(vec, class)]
  return (list(data=data, features=vec))
}

# Relief

fs.relief <- function(data, class)
{
  class_formula = formula(paste(class, "  ~ ."))
  weights = relief(class_formula, data)
  
  tab=data.frame(weights)
  tab=orderBy(~-attr_importance, data=tab)
  tab$i=row(tab)
  tab$import_acum=cumsum(tab$attr_importance)
  res = curvature.min(tab$i, tab$import_acum)
  tab = tab[tab$import_acum <= res$y,]
  vec = rownames(tab)
  data = data[,c(vec, class)]
  return (list(data=data, features=vec))
}

