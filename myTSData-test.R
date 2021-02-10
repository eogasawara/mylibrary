
load_series <- function(name) {
  link <- url(sprintf("https://raw.githubusercontent.com/eogasawara/mylibrary/master/data/time-series/%s.RData", name))
  x <- get(load(link))
  return(x)  
}

x <- load_series("sin")


test_sw <- function(x, sw) {
  ts <- ts_data(x, sw)
  print(head(ts$data))
  
  sample <- ts_sample(ts)
  sample <- train_test(sample)
  print(head(sample$train))
  
  norm <- ts_gminmax_diff(sample$train)
  norm <- prepare(norm)
  norm <- action(norm)
  print(head(norm$data))
  normd <- deaction(norm)
  print(head(normd$data))
  
  ts$data <- norm$data
  ts <- ts_projection(ts)
  print(head(ts$input))
}

test_sw(x, 0)

test_sw(x, 10)
