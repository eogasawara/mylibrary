loadlibrary <- function(x, repos='http://cran.fiocruz.br') 
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x, repos=repos, dep=TRUE)
    require(x)
  }
}

# curvature


curvature <- function(x, y, func) {
  obj <- list(x = x, y = y, df=2, deriv=2)
  attr(obj, "class") <- "curvature"
  obj <- fit(obj, func)
  return(obj)
}

fit <- function(obj, func) {
  #x contains both input and output
  UseMethod("fit")
}

fit.default <- function(obj, func) {
  return(obj)
}

fit.curvature <- function(obj, func) {
  smodel = smooth.spline(obj$x, obj$y, df = obj$df)
  curvature = predict(smodel, x = obj$x, deriv = obj$deriv)
  obj$yfit = func(curvature$y)
  obj$xfit = match(obj$yfit, curvature$y)
  
  res = data.frame(x=obj$x[obj$xfit], y=obj$y[obj$xfit], z=obj$yfit)
  obj$res <- res
  return(obj)
}

plot.curvature <- function(obj) {
  plot(obj$x, obj$y,col=ifelse(obj$x==obj$xfit, "red", "black"))   
}


