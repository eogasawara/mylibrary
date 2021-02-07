# version 1.0
# debug: setwd("C:/Users/eduar/OneDrive/Git/mylibrary")
myrepos <- "http://cran.fiocruz.br"


setrepos <- function(repos=repos) {
  myrepos <- repos 
}

loadlibrary <- function(x) 
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x, repos=myrepos, dep=TRUE)
    require(x)
  }
}

obj_transform <- function(data) {
  obj <- list(data=data)
  attr(obj, "class") <- "obj_transform"  
  return(obj)
}

action <- function(obj) {
  UseMethod("action")
}

action.default <- function(obj) {
  return(NULL)
}

action.obj_transform <- function(obj) {
  return(obj$data)
}

prepare <- function(obj) {
  UseMethod("prepare")
}

prepare.default <- function(obj) {
  return(obj)
}

optimize <- function(obj, ...) {
  UseMethod("optimize")
}

optimize.default <- function(obj, ...) {
  return(obj)
}

start_log <- function(obj) {
  UseMethod("start_log")
}

start_log.default <- function(obj) {
  obj$log_time <- Sys.time()
  return(obj)
}

register_log <- function(obj, msg, ref) {
  UseMethod("register_log")
}

register_log.default <- function(obj, msg = "") {
  obj$log_time <- sprintf("%.3f", difftime(Sys.time(), obj$log_time, units = "min"))
  ref <- deparse(sys.calls()[[sys.nframe()-2]])
  if (is.null(ref))
    ref <- class(obj)
  if (is.null(ref))
    ref <- ""
  obj$log_msg <- sprintf("%s,%s,%s", ref, obj$log_time, msg)
  message(obj$log_msg)
  return(obj)
}



rel_transform <- function(data) {
  obj <- obj_transform(data)
  class(obj) <- append("rel_transform", class(obj))    
  return(obj)
}

atr_transform <- function(data) {
  obj <- obj_transform(data)
  class(obj) <- append("atr_transform", class(obj))    
  return(obj)
}

