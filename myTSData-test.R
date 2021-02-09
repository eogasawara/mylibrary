
load_series <- function(name) {
  link <- url(sprintf("https://raw.githubusercontent.com/eogasawara/mylibrary/master/data/time-series/%s.RData", name))
  x <- get(load(link))
  return(x)  
}

x <- load_series("sin")

ts <- ts_data(x)

ts <- prepare(ts)

res <- action(ts)

ts <- train_test(ts, test_size=10)

ts <- sw_project(ts)

print(res)



ts <- ts_data(x, 10)

ts <- prepare(ts)

res <- action(ts)

ts <- train_test(ts, test_size=10)

ts <- sw_project(ts)

print(res)

