#converter ts_data para matrix
#definir método para não perder a classe e os atributos
#https://stackoverflow.com/questions/7532845/matrix-losing-class-attribute-in-r

load_series <- function(name) {
  link <- url(sprintf("https://raw.githubusercontent.com/eogasawara/mylibrary/master/data/time-series/%s.RData", name))
  x <- get(load(link))
  return(x)  
}

x <- load_series("sin")


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


#ts_data
ts_data <- function(data, sw=0) {
  if (sw > 1) {
    data <- ts_sw(as.matrix(data), sw)  
  }
  else {
    data <- as.matrix(data)
    sw <- 0
  }
  
  col <- paste("t",(ncol(data)-1):0, sep="")
  colnames(data) <- col
  
  class(data) <- append("ts_data", class(data))    
  attr(data, "sw") <- sw  
  return(data)
}

`[.ts_data` <- function(x, i, j, ...) {
  y <- unclass(x)[i,j,...]
  missing(i)
  missing(j)
  (se i e j é um é um valor)
  class(y) <- c("ts_data",class(y))
  return(y)
}

data <- ts_data(x)

data10 <- ts_data(x, 10)

r1 <- data10[12,]
r2 <- data10[12:13,]

c1 <- data10[,1]
c2 <- data10[,1:2]


