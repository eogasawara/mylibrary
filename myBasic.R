# version 1.2

if (is.null(repos_name))
  repos_name <- getOption("repos")[1]

setrepos <- function(repos=repos) {
  repos_name <- repos 
}

loadlibrary <- function(packagename) 
{
  if (!require(packagename, character.only = TRUE))
  {
    install.packages(packagename, repos=repos_name, dep=TRUE, verbose = FALSE)
    require(packagename, character.only = TRUE)
  }
}


### basic data strucutures


# general functions
adjust.matrix <- function(data) {
  if(!is.matrix(data)) {
    return(as.matrix(data))
  }
  else
    return(data)
}

adjust.data.frame <- function(data) {
  if(!is.data.frame(data)) {
    return(as.data.frame(data))
  }
  else
    return(data)
}



ts_data <- function(y, sw=1) {
  #https://stackoverflow.com/questions/7532845/matrix-losing-class-attribute-in-r
  ts_sw <- function(x, sw) {
    ts_lag <- function(x, k) 
    {
      c(rep(NA, k), x)[1 : length(x)] 
    }
    n <- length(x)-sw+1
    window <- NULL
    for(c in (sw-1):0){
      t  <- ts_lag(x,c)
      t <- t[sw:length(t)]
      window <- cbind(window,t,deparse.level = 0)
    }
    col <- paste("t",c((sw-1):0), sep="")
    colnames(window) <- col
    return(window)  
  }
  
  if (sw > 1) 
    y <- ts_sw(as.matrix(y), sw)  
  else {
    y <- as.matrix(y)
    sw <- 1
  }
  
  col <- paste("t",(ncol(y)-1):0, sep="")
  colnames(y) <- col
  
  class(y) <- append("ts_data", class(y))    
  attr(y, "sw") <- sw  
  return(y)
}

`[.ts_data` <- function(x, i, j, ...) {
  y <- unclass(x)[i,j,...]
  if (is.matrix(y)) {
    class(y) <- c("ts_data",class(y))
    attr(y, "sw") <- ncol(y)
  }
  else {
    if (length(y) > 1) {
      if (is.null(names(y))) 
        dim(y) <- c(length(y), 1)
      else
        dim(y) <- c(1, length(y))
      class(y) <- c("ts_data",class(y))
      attr(y, "sw") <- ncol(y)
    }
  }
  return(y)
}


#ts_sample

ts_sample <- function(ts, test_size=1, offset=0) {
  offset <- nrow(ts) - test_size - offset
  train <- ts[1:offset, ]
  test <- ts[(offset+1):(offset+test_size),]
  colnames(test) <- colnames(train)
  samp <- list(train = train, test = test)
  attr(samp, "class") <- "ts_sample"  
  return(samp)
}


#ts_projection

ts_projection <- function(ts) {
  input <- ts
  output <- ts
  
  if (is.matrix(ts) || is.data.frame(ts)) {
    if (nrow(ts) > 1) {
      input <- ts[,1:(ncol(ts)-1)]
      colnames(input) <- colnames(ts)[1:(ncol(ts)-1)]
      output <- ts[,ncol(ts)]
      colnames(output) <- colnames(ts)[ncol(ts)]
    }
    else {
      input <- ts_data(ts[,1:(ncol(ts)-1)], ncol(ts)-1)
      colnames(input) <- colnames(ts)[1:(ncol(ts)-1)]
      output <- ts_data(ts[,ncol(ts)], 1)
      colnames(output) <- colnames(ts)[ncol(ts)]
    }
  }
  
  proj <- list(input = input, output = output)
  attr(proj, "class") <- "ts_projection"  
  return(proj)
}


### basic transformation functions

dal_transform <- function() {
  obj <- list()
  attr(obj, "class") <- "dal_transform"  
  return(obj)
}

#action

action <- function(obj, ...) {
  UseMethod("action")
}

action.default <- function(obj) {
  return(NULL)
}

#deaction

deaction <- function(obj, ...) {
  UseMethod("action")
}

deaction.default <- function(obj) {
  return(NULL)
}

#prepare

prepare <- function(obj, ...) {
  UseMethod("prepare")
}

prepare.default <- function(obj) {
  return(obj)
}

#optimize

optimize <- function(obj, ...) {
  UseMethod("optimize")
}

optimize.default <- function(obj) {
  return(obj)
}

#start_log

start_log <- function(obj) {
  UseMethod("start_log")
}

start_log.default <- function(obj) {
  obj$log_time <- Sys.time()
  return(obj)
}

#register_log

register_log <- function(obj, msg, ref) {
  UseMethod("register_log")
}

register_log.default <- function(obj, msg = "") {
  obj$log_time <- as.numeric(difftime(Sys.time(), obj$log_time, units = "min"))
  ref <- as.list(sys.call(-2))[[1]]
  if (is.null(ref))
    ref <- ""
  else
    ref <- as.character(ref)
  obj$log_msg <- sprintf("%s,%s,%.3f,%s", as.character(class(obj)[1]), ref, obj$log_time, msg)
  message(obj$log_msg)
  return(obj)
}