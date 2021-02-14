# version 1.0
source("myData.R")

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
