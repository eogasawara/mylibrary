loadlibrary <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x, repos='http://cran.fiocruz.br', dep=TRUE)
  }
  require(x,character.only = TRUE)
}

loadlibrary("caret")
loadlibrary("MASS")
loadlibrary("glmnet")
loadlibrary("leaps")
loadlibrary("FSelector")
loadlibrary("doBy")

# samples

sample.random <- function(data, perc=0.8)
{
  idx = sample(1:nrow(data),as.integer(perc*nrow(data)))
  sample = data[idx,]
  residual = data[-idx,]
  return (list(sample=sample, residual=residual))
}

sample.stratified <- function(data, clabel, perc=0.8)
{
  predictors_name  = setdiff(colnames(data), clabel)
  
  predictors = data[,predictors_name] 
  predictand = data[,clabel] 
  
  idx = createDataPartition(predictand, p=perc, list=FALSE)  
  sample = data[idx,]
  residual = data[-idx,]
  return (list(sample=sample, residual=residual))
}

sample.random_kfold <- function(data, k=10)
{
  sets = list()
  p = 1.0 / k
  while (k > 1) {
    samples = sample.random(data, p)
    fold = samples[[1]]
    data = samples[[2]]
    sets = append(sets, list(fold))
    k = k - 1
    p = 1.0 / k
  }
  sets = append(sets, list(data))
  return (sets)
}

sample.stratified_kfold <- function(data, clabel, k=10)
{
  sets = list()
  p = 1.0 / k
  while (k > 1) {
    samples = sample.stratified(data, clabel, p)
    fold = samples[[1]]
    data = samples[[2]]
    sets = append(sets, list(fold))
    k = k - 1
    p = 1.0 / k
  }
  sets = append(sets, list(data))
  return (sets)
}


# outlier analysis

outliers.boxplot <- function(data, alpha = 1.5)
{
  out = rep(FALSE, nrow(data))
  nums = unlist(lapply(data, is.numeric))
  for (i in 1:ncol(data))
  {
    if (nums[i])
    {
      q = quantile(data[,i])
      IQR = q[4] - q[2]
      lq1 = q[2] - alpha*IQR
      hq3 = q[4] + alpha*IQR
      out = out | data[,i] < lq1 | data[,i] > hq3
    }
  }
  return (out)
}


# curvature analysis

curvature.max <- function(x, y, df=3) {
  smodel = smooth.spline(x, y, df = df)
  curvature = predict(smodel, x = x, deriv = 2)
  yv = max(curvature$y)
  xv = match(yv,curvature$y)
  plot(x, y)
  points(x[xv], y[xv], pch=19)
  res = data.frame(x[xv], y[xv], yv)
  colnames(res) = c("x", "y", "z")
  return (res)
}

curvature.min <- function(x, y, df=3) {
  smodel = smooth.spline(x, y, df = df)
  curvature = predict(smodel, x = x, deriv = 2)
  yv = min(curvature$y)
  xv = match(yv,curvature$y)
  plot(x, y)
  points(x[xv], y[xv], pch=19)
  res = data.frame(x[xv], y[xv], yv)
  colnames(res) = c("x", "y", "z")
  return (res)
}


# min-max normalization

normalize.minmax <- function(data, norm.set=NULL)
{
  data = data.frame(data)
  nums = unlist(lapply(data, is.numeric))
  data = data[ , nums]
  
  if(is.null(norm.set))
  {
    minmax = data.frame(t(sapply(data, max, na.rm=TRUE)))
    minmax = rbind(minmax, t(sapply(data, min, na.rm=TRUE)))
    colnames(minmax) = colnames(data)    
    rownames(minmax) = c("max", "min")
  }
  else {
    minmax = norm.set
  }
  for (i in 1:ncol(data))
    data[,i] = (data[,i] - minmax["min", i]) / (minmax["max", i] - minmax["min", i])
  return (list(data=data, norm.set=minmax))
}


# z-score normalization

normalize.zscore <- function(data, norm.set=NULL, nmean=0, nsd=1)
{
  data = data.frame(data)
  nums = unlist(lapply(data, is.numeric))
  data = data[ , nums]

  if(is.null(norm.set))
  {
    zscore <- matrix(nrow = 4, ncol=ncol(data))
    zscore[1,] = t(sapply(data, mean, na.rm=TRUE))
    zscore[2,] = t(sapply(data, sd, na.rm=TRUE))
    zscore[3,] = t(rep(nmean, ncol(data)))
    zscore[4,] = t(rep(nsd, ncol(data)))
    zscore = data.frame(zscore)
    colnames(zscore) = colnames(data)    
    rownames(zscore) = c("mean", "sd","nmean", "nsd")
  }
  for (i in 1:ncol(data))
    data[,i] = ((data[,i] - zscore["mean", i]) / zscore["sd", i]) * zscore["nsd", i] + zscore["nmean", i]
  return (list(data=data, norm.set=zscore))
}


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



