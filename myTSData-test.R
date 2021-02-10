source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myTSData.R")
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/mySample.R")


load_series <- function(name) {
  link <- url(sprintf("https://raw.githubusercontent.com/eogasawara/mylibrary/master/data/time-series/%s.RData", name))
  x <- get(load(link))
  return(x)  
}

x <- load_series("sin")


test_sw <- function(x, sw, norm) {
  ts <- ts_data(x, sw)
  print("org data")
  print(head(ts$data))
  
  sample <- ts_sample(ts)
  sample <- train_test(sample)
  print("sample data")
  print(head(sample$train))
  
  norm$data <- sample$train
  norm <- prepare(norm)
  norm <- action(norm, x)
  print("normalized data")
  print(head(norm$data))
  
  tsproj <- ts
  tsproj$data <- norm$data
  tsproj <- ts_projection(tsproj)
  print("projection of normalized data")
  print(head(tsproj$input))

  norm <- deaction(norm)
  print("denormalized data")
  print(head(norm$data))
  
}

#test_sw(x, 0, ts_gminmax())
test_sw(x, 10, ts_swminmax())
#test_sw(x, 0, ts_gminmax_diff())
#test_sw(x, 10, ts_gminmax_diff())
