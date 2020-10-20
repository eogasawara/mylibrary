source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myOutlier.R")

loadlibrary <- function(x, repos='http://cran.fiocruz.br') 
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x, repos=repos, dep=TRUE)
    require(x)
  }
}

# basic functions

ts_sw <- function(x, sw_size) {
  ts_lag <- function(x, k) 
  {
    c(rep(NA, k), x)[1 : length(x)] 
  }
  n <- length(x)-sw_size+1
  sw <- NULL
  for(c in (sw_size-1):0){
    t  <- ts_lag(x,c)
    t <- t[sw_size:length(t)]
    sw <- cbind(sw,t,deparse.level = 0)
  }
  col <- paste("t",c((sw_size-1):0), sep="")
  #rownames(sw) <- NULL
  colnames(sw) <- col
  return(sw)  
}

ts_as_matrix <- function(sw, size) {
  sw <- data.frame(sw)
  sw <- sw[, (ncol(sw)-size+1):ncol(sw)]
  sw <- as.matrix(sw)
  return(sw)
}

ts_train_test <- function(x, test_size, sw_size = 0, offset=0) {
  if (offset == 0) {
    offset <- length(x)-test_size
  }
  if (sw_size == 0) {
    train <- x[1:offset]
    test <- x[(offset+1):(offset+test_size)]
  }
  else {
    train <- x[1:offset]
    test <- x[(offset-(sw_size-1)+1):(offset+test_size)]
    train <- ts_sw(train, sw_size)
    test <- ts_sw(test, sw_size)
  }
  return(list(train=train, test=test))
}

ts_sw_project <- function(sw) 
{
  if (is.vector(sw)) {
    input <- sw
    output <- sw
  }
  else {
    input <- sw[,1:ncol(sw)-1]
    output <- sw[,ncol(sw)]
  }
  return (list(input=input, output=output))
} 

# time series preprocessing

# classe ts_preprocess

ts_preprocess <- function() {
  value <- list(sw_size = NA, scale = 1, offset = 0, rescale = FALSE)
  attr(value, "class") <- "ts_preprocess"
  return(value)
}


ts_setup <- function(obj, x) {
  #x contains both input and output
  UseMethod("ts_setup")
}

ts_setup.default <- function(obj, x) {
  if (is.vector(x)) 
    obj$sw_size <- 0
  else
    obj$sw_size <- ncol(x)
  return(obj)
}

ts_normalize <- function(obj, x, arguments=NULL) {
  # x can be either input matrix or output vector
  UseMethod("ts_normalize")
}

ts_normalize.default <- function(obj, x, arguments=NULL) {
  # x can be either input matrix or output vector
  return(list(x=x,arguments=NULL))
}

ts_denormalize <- function(obj, x, arguments=NULL) {
  # x can be either input matrix or output vector
  UseMethod("ts_denormalize")
}

ts_denormalize.default <- function(obj, x, arguments=NULL) {
  # x can be either input matrix or output vector
  return(list(x=x,arguments=NULL))
}

# classe ts_gminmax

ts_gminmax <- function() {
  value <- ts_preprocess()
  value$gmin <- NaN
  value$gmax <- NaN
  value$rescale <- TRUE
  class(value) <- append("ts_gminmax", class(value))  
  return(value)
}

ts_setup.ts_gminmax <- function(obj, x) {
  obj <- ts_setup.default(obj, x)
  
  x <- action(outliers(x))   
  
  io <- ts_sw_project(x)
  
  obj$gmin <- min(x)
  obj$gmax <- max(x)
  
  if (obj$rescale) {
    swi_min <- apply(io$input, 1, min)
    swi_max <- apply(io$input, 1, max)
    
    swio_min <- apply(x, 1, min)
    swio_max <- apply(x, 1, max)
    
    ratio <- (swi_max-swi_min)/(swio_max-swio_min)
    ratio <- action(outliers(ratio))
    ratio <- mean(ratio)
    
    w <- (obj$gmax - obj$gmin)/(2*ratio)
    c <- (obj$gmax + obj$gmin)/2
    obj$gmax <- c + w
    obj$gmin <- c - w
  }
  
  return(obj)
}

ts_normalize.ts_gminmax <- function(obj, x, arguments=NULL) {
  # x can be either input matrix or output vector
  return (list(x = obj$scale*(x-obj$gmin)/(obj$gmax-obj$gmin) + obj$offset, arguments = NULL))
}

ts_denormalize.ts_gminmax <- function(obj, x, arguments=NULL) {
  # x can be either input matrix or output vector
  return (list(x=((x - obj$offset) * (obj$gmax-obj$gmin) + obj$gmin), arguments=NULL))
}

# classe ts_diff

ts_gminmax_diff <- function() {
  value <- ts_preprocess()
  value$gmin <- NaN
  value$gmax <- NaN
  value$rescale <- TRUE
  class(value) <- append("ts_gminmax_diff", class(value))  
  return(value)
}

ts_diff <- function(x) {
  isvector <- !(is.matrix(x) || is.data.frame(x))
  if(isvector) {
    x <- ts_sw(x,2)
  }
  x <- x[,2:ncol(x)] - x[,1:(ncol(x)-1)]
  return(x)
}

ts_setup.ts_gminmax_diff <- function(obj, x) {
  obj <- ts_setup.default(obj, x)
  
  x <- ts_diff(x)
  
  x <- action(outliers(x))   
  
  io <- ts_sw_project(x)
  
  obj$gmin <- min(x)
  obj$gmax <- max(x)
  
  if (obj$rescale) {
    swi_min <- apply(io$input, 1, min)
    swi_max <- apply(io$input, 1, max)
    
    swio_min <- apply(x, 1, min)
    swio_max <- apply(x, 1, max)
    
    ratio <- (swi_max-swi_min)/(swio_max-swio_min)
    ratio <- action(outliers(ratio))
    ratio <- mean(ratio)
    
    obj$offset <- obj$offset + (1 - ratio) * obj$scale / 2
    obj$scale <- obj$scale * ratio
  }
  
  return(obj)
}

ts_normalize.ts_gminmax_diff <- function(obj, x, arguments=NULL) {
  # x can be either input matrix or output vector
  if (is.vector(x)) {
    ref <- arguments$ref
    x <- x - ref
  }
  else {
    ref <- x[,ncol(x)]
    x <- ts_diff(x)
  }
  x <- obj$scale*(x-obj$gmin)/(obj$gmax-obj$gmin) + obj$offset
  return (list(x=x, arguments=list(ref=ref)))
}

ts_denormalize.ts_gminmax_diff <- function(obj, x, arguments=NULL) {
  # x can be either input matrix or output vector
  x <- (x - obj$offset) * (obj$gmax-obj$gmin) / obj$scale + obj$gmin
  if (is.vector(x)) {
    ref <- arguments$ref
    x <- x + ref
  }
  else {
    ref <- arguments$ref
    x <- cbind(x, arguments$ref)
    for (i in (ncol(x)-1):1) {
      x[,i] <- x[,i+1]-x[,i]
    }
  }
  return (list(x=x, arguments=list(ref=ref)))
}

# classe ts_swminmax

ts_swminmax <- function() {
  value <- ts_preprocess()
  value$rescale <- TRUE
  class(value) <- append("ts_swminmax", class(value))  
  return(value)
}

ts_setup.ts_swminmax <- function(obj, x) {
  valid_range <- function(x, alpha=1.5) {
    q <- quantile(x)
    IQR <- q[4] - q[2]
    return(q[2] - alpha*IQR)
  }
  
  obj <- ts_setup.default(obj, x)
  
  x <- action(outliers(x))   
  
  io <- ts_sw_project(x)
  
  if (obj$rescale) {
    swi_min <- apply(io$input, 1, min)
    swi_max <- apply(io$input, 1, max)
    
    swio_min <- apply(x, 1, min)
    swio_max <- apply(x, 1, max)
    
    ratio <- (swi_max-swi_min)/(swio_max-swio_min)
    ratio <- action(outliers(ratio))
    ratio <- valid_range(ratio)
    obj$offset <- obj$offset + (1 - ratio) * obj$scale / 2
    obj$scale <- obj$scale * ratio
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
  return (list(x=obj$scale*(x-i_min)/(i_max-i_min) + obj$offset, arguments=list(i_min=i_min, i_max=i_max)))
}

ts_denormalize.ts_swminmax <- function(obj, x, arguments=NULL) {
  # x can be either input matrix or output vector
  x <- (x - obj$offset) * (arguments$i_max - arguments$i_min) / obj$scale + arguments$i_min
  return (list(x=x, arguments=arguments))
}

# classe ts_anminmax

ts_anminmax <- function() {
  value <- ts_preprocess()
  value$gmin <- NA
  value$gmax <- NA
  value$rescale <- TRUE
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

ts_setup.ts_anminmax <- function(obj, x) {
  obj <- ts_setup.default(obj, x)
  
  an <- ts_inertia(obj, ts_sw_project(x)$input)
  x <- ts_remove_inertia(obj, x, an)
  x <- cbind(x, an)
  x <- action(outliers(x))   
  x <- x[,1:(ncol(x)-1)]
  
  io <- ts_sw_project(x)
  
  obj$gmin <- min(io$input)
  obj$gmax <- max(io$input)
  
  if (obj$rescale) {
    ratio <- (obj$gmax-obj$gmin)/(max(x)-min(x))
    
    obj$offset <- obj$offset + (1 - ratio) * obj$scale / 2
    obj$scale <- obj$scale * ratio
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
  return (list(x=obj$scale*(x - obj$gmin)/(obj$gmax - obj$gmin) + obj$offset, arguments=list(an=an)))
}

ts_denormalize.ts_anminmax <- function(obj, x, arguments=NULL) {
  # x can be either input matrix or output vector
  x <- (x - obj$offset) * (obj$gmax - obj$gmin) / (obj$scale) + obj$gmin
  x <- ts_add_inertia(obj, x, arguments$an)
  return (list(x=x, arguments=arguments))
}

# classe ts_animinmax

ts_animinmax <- function() {
  value <- ts_anminmax()
  value$gmin <- NA
  value$gmax <- NA
  value$rescale <- TRUE
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
  value$rescale <- TRUE
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


