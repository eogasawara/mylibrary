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

outliers <- function(data, alpha=1.5) {
  if(is.matrix(data) || is.data.frame(data)) {
    idx = rep(FALSE, nrow(data))
    org <- nrow(data)
    data <- as.data.frame(data)
    if (org >= 30) {
      isnumeric = (ifelse(sapply(data, is.numeric), TRUE, FALSE))
      data <- data[,as.vector(isnumeric)]
      q <- sapply(data, quantile, na.rm=TRUE)
      n <- ncol(data)
      for (i in 1:n)
      {
        IQR <- q[4,i] - q[2,i]
        lq1 <- q[2,i] - alpha*IQR
        hq3 <- q[4,i] + alpha*IQR
        idx = idx | (!is.na(data[,i]) & (data[,i] < lq1 | data[,i] > hq3))
      }
    }
    return(idx)
  }
  else {
    idx <- rep(FALSE, length(data))
    if (length(data) >= 30) {
      q <- quantile(data)
      IQR <- q[4] - q[2]
      lq1 <- q[2] - obj$alpha*IQR
      hq3 <- q[4] + obj$alpha*IQR
      idx <- data < lq1 | data > hq3
    }
    return (idx) 
  } 
}

outliers_remove <- function(data)
{
  idx <- outliers(data)
  if(is.matrix(data) || is.data.frame(data)) {
    return(data[!idx,])
  }
  else {
    return(data[!idx])
  } 
}

an_inertia <- function(x) {
  an <- apply(x, 1, mean)
  return(an)
}

an_remove_inertia <- function(x, an) {
  return(x / an)
}

an_add_inertia <- function(x, an) {
  return(x * an)
}

AN_setup <- function(x, rescale=TRUE) {
  obj <- list(sw_size <- ncol(x), rescale=rescale, offset=0, scale=1)
  
  an <- an_inertia(ts_sw_project(x)$input)
  x <- an_remove_inertia(x, an)
  x <- cbind(x, an)
  x <- outliers_remove(x)    
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

AN_normalize <- function(obj, x, arguments=NULL) {
  # x can be either input matrix or output vector
  if (is.vector(x)) {
    an <- arguments$an
    x <- an_remove_inertia(x, an)
  }
  else {
    an <- an_inertia(x)
    x <- an_remove_inertia(x, an)
  }
  return (list(x=obj$scale*(x - obj$gmin)/(obj$gmax - obj$gmin) + obj$offset, arguments=list(an=an)))
}

AN_denormalize <- function(obj, x, arguments=NULL) {
  # x can be either input matrix or output vector
  x <- (x - obj$offset) * (obj$gmax - obj$gmin) / (obj$scale) + obj$gmin
  x <- an_add_inertia(x, arguments$an)
  return (list(x=x, arguments=arguments))
}


library(nnet)

load_series <- function(name) {
  link <- url(sprintf("https://raw.githubusercontent.com/eogasawara/mylibrary/master/data/time-series/%s.RData", name))
  x <- get(load(link))
  return(x)  
}

x <- load_series("sin")

tt <- ts_train_test(x, test_size=12, sw_size=36)
head(tt$train)
AN <- AN_setup(tt$train)

io <- ts_sw_project(tt$train)

input <- AN_normalize(AN,io$input)

output <- AN_normalize(AN, io$output, arguments=input$arguments)

print(sprintf("i=[%.3f,%.3f] - o=[%.3f,%.3f]", min(input$x), max(input$x), min(output$x), max(output$x)))

mdl <- nnet(ts_as_matrix(input$x, 12), output$x, size=12)

plot(output$x, main="AN: output during training")

input_org <- AN_denormalize(AN, input$x, arguments=input$arguments)
output_org <- AN_denormalize(AN, output$x, arguments=input$arguments)

print(sprintf("i=%.3f - o=%.3f", abs(mean(input_org$x-io$input)), abs(mean(output_org$x-io$output))))


io <- ts_sw_project(tt$test)
input <- AN_normalize(AN, io$input)
pred_norm <- predict(mdl, ts_as_matrix(input$x, 12))

pred <- AN_denormalize(AN, pred_norm, arguments=input$arguments)

plot(io$output, main="prediction")
lines(pred$x, col="red")
