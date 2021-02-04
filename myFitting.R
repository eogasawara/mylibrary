# version 1.0
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myRelation.R")

# curvature


curvature <- function(x, y, func) {
  obj <- list(x = x, y = y, df=2, deriv=2, func=func)
  attr(obj, "class") <- "curvature"
  return(obj)
}

fit <- function(obj) {
  #x contains both input and output
  UseMethod("fit")
}

fit.default <- function(obj) {
  return(obj)
}

fit.curvature <- function(obj) {
  smodel = smooth.spline(obj$x, obj$y, df = obj$df)
  curvature = predict(smodel, x = obj$x, deriv = obj$deriv)
  obj$yfit = obj$func(curvature$y)
  obj$xfit = match(obj$yfit, curvature$y)
  
  res = data.frame(x=obj$x[obj$xfit], y=obj$y[obj$xfit], z=obj$yfit)
  obj$res <- res
  return(obj)
}

plot.curvature <- function(obj) {
  plot(obj$x, obj$y,col=ifelse(obj$x==obj$xfit, "red", "black"))   
}


