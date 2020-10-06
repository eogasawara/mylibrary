#https://www.datamentor.io/r-programming/s3-class/

student <- function(n,a,g) {
  # we can add our own integrity checks
  if(g>4 || g<0)  stop("GPA must be between 0 and 4")
  value <- list(name = n, age = a, GPA = g)
  # class can be set using class() or attr() function
  attr(value, "class") <- "student"
  return(value)
}

professor <- function(n,a) {
  value <- student(n, a, 4)
# attr(value, "class") <- "professor"
  class(value) <- append("professor", class(value))  
  return(value)
}

print.student <- function(obj) {
  cat(obj$name, "\n")
  cat(obj$age, "years old\n")
  cat("GPA:", obj$GPA, "\n")
}

print.professor <- function(obj) {
  cat(obj$name, "\n")
  cat(obj$age, "years old\n")
}



grade <- function(obj) {
  UseMethod("grade")
}

grade.default <- function(obj) {
  cat("This is a generic function\n")
}


grade.student <- function(obj) {
  cat("Your grade is", obj$GPA, "\n")
}


a <- 3
s <- student("Paul", 26, 3.7)
p <- professor("Eduardo", 46)
print(a)
print(s)
print(p)

grade(a)
grade(s)
grade(p)


methods(class="default")


