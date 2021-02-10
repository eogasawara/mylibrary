# classe ts_swminmax

ts_swminmax <- function() {
  value <- ts_preprocess()
  value$scale <- TRUE
  class(value) <- append("ts_swminmax", class(value))  
  return(value)
}

ts_normalize.ts_swminmax <- function(obj, x) {
  valid_range <- function(x, alpha=1.5) {
    q <- quantile(x)
    IQR <- q[4] - q[2]
    return(q[2] - alpha*IQR)
  }
  
  obj <- ts_normalize.default(obj, x)
  
  x <- action(outliers(x))   
  
  io <- ts_sw_project(x)
  
  if (obj$scale) {
    swi_min <- apply(io$input, 1, min)
    swi_max <- apply(io$input, 1, max)
    
    swio_min <- apply(x, 1, min)
    swio_max <- apply(x, 1, max)
    
    ratio <- (swi_max-swi_min)/(swio_max-swio_min)
    ratio <- action(outliers(ratio))
    ratio <- valid_range(ratio)
    obj$scale_offset <- obj$scale_offset + (1 - ratio) * obj$scale_factor / 2
    obj$scale_factor <- obj$scale_factor * ratio
  }
  
  return(obj)
}

ts_normalize.ts_swminmax <- function(obj, x, arguments=NULL) {
  # x can be either input matrix or output vector
  if (is.vector(x)) {
    i_min <- arguments$i_min 
    i_max <- arguments$i_max
  }
  else {
    i_min <- apply(x, 1, min)
    i_max <- apply(x, 1, max)
  }
  return (list(x=obj$scale_factor*(x-i_min)/(i_max-i_min) + obj$scale_offset, arguments=list(i_min=i_min, i_max=i_max)))
}

ts_denormalize.ts_swminmax <- function(obj, x, arguments=NULL) {
  # x can be either input matrix or output vector
  x <- (x - obj$scale_offset) * (arguments$i_max - arguments$i_min) / obj$scale_factor + arguments$i_min
  return (list(x=x, arguments=arguments))
}

# classe ts_anminmax

ts_anminmax <- function() {
  value <- ts_preprocess()
  value$gmin <- NA
  value$gmax <- NA
  value$scale <- TRUE
  class(value) <- append("ts_anminmax", class(value))  
  return(value)  
}

ts_inertia <- function(obj, x) {
  UseMethod("ts_inertia")
}

ts_inertia.default <- function(obj, x) {
  an <- apply(x, 1, mean)
  return(an)
}

ts_remove_inertia <- function(obj, x, an) {
  UseMethod("ts_remove_inertia")
}

ts_remove_inertia.default <- function(obj, x, an) {
  return(x / an)
}

ts_add_inertia <- function(obj, x, an) {
  UseMethod("ts_add_inertia")
}

ts_add_inertia.default <- function(obj, x, an) {
  return(x * an)
}

ts_normalize.ts_anminmax <- function(obj, x) {
  obj <- ts_normalize.default(obj, x)
  
  an <- ts_inertia(obj, ts_sw_project(x)$input)
  x <- ts_remove_inertia(obj, x, an)
  x <- cbind(x, an)
  x <- action(outliers(x))   
  x <- x[,1:(ncol(x)-1)]
  
  io <- ts_sw_project(x)
  
  obj$gmin <- min(io$input)
  obj$gmax <- max(io$input)
  
  if (obj$scale) {
    ratio <- (obj$gmax-obj$gmin)/(max(x)-min(x))
    
    obj$scale_offset <- obj$scale_offset + (1 - ratio) * obj$scale_factor / 2
    obj$scale_factor <- obj$scale_factor * ratio
  }
  return(obj)
}

ts_normalize.ts_anminmax <- function(obj, x, arguments=NULL) {
  # x can be either input matrix or output vector
  if (is.vector(x)) {
    an <- arguments$an
    x <- ts_remove_inertia(obj, x, an)
  }
  else {
    an <- ts_inertia(obj, x)
    x <- ts_remove_inertia(obj, x, an)
  }
  return (list(x=obj$scale_factor*(x - obj$gmin)/(obj$gmax - obj$gmin) + obj$scale_offset, arguments=list(an=an)))
}

ts_denormalize.ts_anminmax <- function(obj, x, arguments=NULL) {
  # x can be either input matrix or output vector
  x <- (x - obj$scale_offset) * (obj$gmax - obj$gmin) / (obj$scale_factor) + obj$gmin
  x <- ts_add_inertia(obj, x, arguments$an)
  return (list(x=x, arguments=arguments))
}

# classe ts_animinmax

ts_animinmax <- function() {
  value <- ts_anminmax()
  value$gmin <- NA
  value$gmax <- NA
  value$scale <- TRUE
  class(value) <- append("ts_animinmax", class(value))  
  return(value)  
}

ts_remove_inertia.ts_animinmax <- function(obj, x, an) {
  return(x - an)
}

ts_add_inertia.ts_animinmax <- function(obj, x, an) {
  return(x + an)
}

ts_aneminmax <- function() {
  value <- ts_anminmax()
  value$gmin <- NA
  value$gmax <- NA
  value$scale <- TRUE
  class(value) <- append("ts_aneminmax", class(value))  
  return(value)  
}

ts_inertia.ts_aneminmax <- function(obj, x) {
  exp_mean <- function(x) {
    n <- length(x)
    y <- rep(0,n)
    alfa <- 1 - 2.0 / (n + 1);
    for (i in 0:(n-1)) {
      y[n-i] <- alfa^i
    }
    m <- sum(y * x)/sum(y)
    return(m)
  }
  
  an <- apply(x, 1, exp_mean)
  return(an)
}


