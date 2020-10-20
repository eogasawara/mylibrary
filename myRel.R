loadlibrary <- function(x, repos='http://cran.fiocruz.br') 
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x, repos=repos, dep=TRUE)
    require(x)
  }
}

obj_transform <- function(data) {
  obj <- list(data=data)
  attr(obj, "class") <- "obj_transform"  
  obj <- prepare(obj)
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

