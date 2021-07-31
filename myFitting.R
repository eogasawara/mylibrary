# version 1.2
# curvature
# source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myTransform.R")

fit_curvature <- function() {
  obj <- dal_transform()
  obj$df <- 2
  obj$deriv <- 2
  class(obj) <- append("fit_curvature", class(obj))    
  return(obj)
}

action.fit_curvature <- function(obj, y) {
  x <- 1:length(y)
  smodel = smooth.spline(x, y, df = obj$df)
  curvature = predict(smodel, x = x, deriv = obj$deriv)
  yfit = obj$func(curvature$y)
  xfit = match(yfit, curvature$y)
  y <- y[xfit]
  res <- data.frame(x=xfit, y=y, yfit = yfit)
  return(res)
}

plot.fit_curvature <- function(obj, y, res) {
  x <- 1:length(y)
  plot(x, y, col=ifelse(x==res$x, "red", "black"))   
}

fit_curvature_min <- function() {
  obj <- fit_curvature()
  obj$func <- min
  class(obj) <- append("fit_curvature_min", class(obj))    
  return(obj)
}

fit_curvature_max <- function() {
  obj <- fit_curvature()
  obj$func <- max
  class(obj) <- append("fit_curvature_max", class(obj))    
  return(obj)
}


