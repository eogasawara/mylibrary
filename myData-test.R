# version 1.0
#source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myData.R")

load_series <- function(name) {
  link <- url(sprintf("https://raw.githubusercontent.com/eogasawara/mylibrary/master/data/time-series/%s.RData", name))
  x <- get(load(link))
  return(x)  
}

x <- load_series("sin")

data <- ts_data(x)

data10 <- ts_data(x, 10)

r1 <- data10[12,]

r2 <- data10[12:13,]

c1 <- data10[,1]

c2 <- data10[,1:2]

rc1 <- data10[12:13,1:2]

rc2 <- data10[12,1:2]

rc3 <- data10[12:13,1]

rc4 <- data10[12,1]
