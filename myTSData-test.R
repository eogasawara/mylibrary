#source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myData.R")
#source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myNormalization.R")

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
  
  samp <- ts_sample(ts)
  print("sample data")
  print(head(samp$train$data))
  
  norm <- prepare(norm, samp$train$data)
  
  proj <- ts_projection(samp$train)
  
  ninput <- action(norm, proj$input$data)
  print("normalized input")
  print(head(ninput))
  
  if (!is.null(proj$output)) {
    noutput <- action(norm, ninput, proj$output$data)
    print("normalized output")
    print(head(noutput))
  }
  y <- ninput[, ncol(ninput)]
  plot(1:length(y), y)
  
  dinput <- deaction(norm, ninput)
  print("denormalized input")
  print(head(dinput))
  
  if (!is.null(proj$output)) {
    doutput <- deaction(norm, dinput, noutput)
    print("denormalized output")
    print(head(doutput))
  }
  y <- dinput[, ncol(dinput)]
  plot(1:length(y), y)
}

#test_sw(x, 0, ts_gminmax(scale=FALSE))
#test_sw(x, 10, ts_gminmax(scale=FALSE))
#test_sw(x, 0, ts_gminmax_diff(scale=FALSE))
#test_sw(x, 10, ts_gminmax_diff(scale=FALSE))
test_sw(x, 10, ts_swminmax(scale=TRUE))
#test_sw(x, 10, ts_an(scale=TRUE))
