obj_preprocessing <- function(data) {
  obj <- list(data=data)
  attr(obj, "class") <- "obj_preprocessing"  
  return(obj)
}

action <- function(obj) {
  UseMethod("action")
}

action.default <- function(obj) {
  return(obj)
}

rel_preprocessing <- function(data) {
  obj <- obj_preprocessing(data)
  class(obj) <- append("rel_preprocessing", class(obj))    
  return(obj)
}

action <- function(obj) {
  UseMethod("action")
}

action.default <- function(obj) {
  return(obj)
}

atr_preprocessing <- function(data) {
  obj <- obj_preprocessing(data)
  class(obj) <- append("atr_preprocessing", class(obj))    
  return(obj)
}

