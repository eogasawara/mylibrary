# version 1.0
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myRelation.R")

# curvature

fit_curvature <- function(data) {
  obj <- atr_transform(data)
  obj$df <- 2
  obj$deriv <- 2
  class(obj) <- append("fit_curvature", class(obj))    
  return(obj)
}

prepare.fit_curvature <- function(obj) {
  obj$x <- 1:length(obj$data)
  smodel = smooth.spline(obj$x, obj$data, df = obj$df)
  curvature = predict(smodel, x = obj$x, deriv = obj$deriv)
  obj$yfit = obj$func(curvature$y)
  obj$xfit = match(obj$yfit, curvature$y)
  obj$y <- obj$data[obj$xfit]
  return(obj)
}

plot.fit_curvature <- function(obj) {
  plot(obj$x, obj$data, col=ifelse(obj$x==obj$xfit, "red", "black"))   
}

action.fit_curvature <- function(obj) {
  res = data.frame(x=obj$xfit, y=obj$data[obj$xfit], yfit = obj$yfit)
  return(res)  
}

fit_curvature_min <- function(data) {
  obj <- fit_curvature(data)
  obj$func <- min
  class(obj) <- append("fit_curvature_min", class(obj))    
  return(obj)
}

fit_curvature_max <- function(data) {
  obj <- fit_curvature(data)
  obj$func <- max
  class(obj) <- append("fit_curvature_max", class(obj))    
  return(obj)
}


