source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myTimeseries.R")

load_series <- function(name) {
  link <- url(sprintf("https://raw.githubusercontent.com/eogasawara/mylibrary/master/data/time-series/%s.RData", name))
  x <- get(load(link))
  return(x)  
}

loadlibrary("tsutils")

teste_season <- function(x) {
  for (i in 2:60) seasplot(x, m=i)  
}

x <- load_series("sin")
teste_season(x)
