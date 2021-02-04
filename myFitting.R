# version 1.0
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myRelation.R")

# curvature

curvature <- function(data) {
  obj <- atr_transform(data)
  obj$df <- 2
  obj$deriv <- 2
  class(obj) <- append("curvature", class(obj))    
  return(obj)
}

prepare.curvature <- function(obj) {
  obj$x <- 1:length(obj$data)
  smodel = smooth.spline(obj$x, obj$data, df = obj$df)
  curvature = predict(smodel, x = obj$x, deriv = obj$deriv)
  obj$yfit = obj$func(curvature$data)
  obj$xfit = match(obj$yfit, curvature$data)
  return(obj)
}

plot.curvature <- function(obj) {
  plot(obj$x, obj$data, col=ifelse(obj$x==obj$xfit, "red", "black"))   
}

action.curvature <- function(obj) {
  res = data.frame(x=obj$x[obj$xfit], y=obj$data[obj$xfit], yfit=obj$yfit)
  return(res)  
}

curvature_min <- function(data) {
  obj <- curvature(data)
  obj$func <- min
  class(obj) <- append("curvature_min", class(obj))    
  return(obj)
}

curvature_max <- function(data) {
  obj <- curvature(data)
  obj$func <- max
  class(obj) <- append("curvature_max", class(obj))    
  return(obj)
}


