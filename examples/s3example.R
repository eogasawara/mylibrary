#https://www.datamentor.io/r-programming/s3-class/

polygon <- function(n) {
  if(n <= 0)  stop("number of vertices should be greater than zero")
  obj <- list(n = n)
  # class can be set using class() or attr() function
  attr(obj, "class") <- "polygon"
  return(obj)
}

rectangle <- function(w, h) {
  obj <- polygon(4)
  obj$w <- w
  obj$h <- h
  class(obj) <- append("rectangle", class(obj))  
  return(obj)
}

print.polygon <- function(obj) {
  cat(obj$n, "\n")
}

print.rectangle <- function(obj) {
  cat(obj$w, ",", obj$h, "\n")
}

area <- function(obj) {
  UseMethod("area")
}

area.default <- function(obj) {
  return(0)
}

area.rectangle <- function(obj) {
  return(obj$w * obj$h)
}

a <- 3
p <- polygon(5)
r <- rectangle(3, 10)

print(a)
print(p)
print(r)

print(area(a))
print(area(p))
print(area(r))

methods(class="default")

