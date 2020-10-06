source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myTimeseries.R")

load_series <- function(name) {
  link <- url(sprintf("https://raw.githubusercontent.com/eogasawara/mylibrary/master/data/time-series/%s.RData", name))
  x <- get(load(link))
  return(x)  
}

x <- load_series("sin")

general_test <- function(x) {
  y <- outliers.boxplot(x)
  
  tt <- ts_train_test(x, test_size=5)
  
  swx <- ts_sw(tt$train, 5)
  
  swy <- outliers.boxplot(swx)
  
  ttsw <- ts_train_test(x, test_size=5, sw_size=5)
  
  swy <- outliers.boxplot(data.frame(ttsw$train))
  
  y <- ts_diff(x)
  
  ysw <- ts_diff(swx)
}
general_test(x)


preprocess_test <- function(prec, x, size) {
  tt <- ts_train_test(x, test_size=size, sw_size=size)

  prec <- ts_setup(prec, tt$train)
  
  io <- ts_sw_project(tt$train)
  
  input <- ts_normalize(prec, io$input)
  output <- ts_normalize(prec, io$output, arguments=input$arguments)

  print(sprintf("%s i=[%.3f,%.3f] - o=[%.3f,%.3f]", class(prec)[1], min(input$x), max(input$x), min(output$x), max(output$x)))
  
  plot(output$x, main=class(prec)[1])
  
  input_org <- ts_denormalize(prec, input$x, arguments=input$arguments)
  output_org <- ts_denormalize(prec, output$x, arguments=input$arguments)

  print(sprintf("%s i=%.3f - o=%.3f", class(prec)[1], abs(mean(input_org$x-io$input)), abs(mean(output_org$x-io$output))))
}

preprocess_test(ts_gminmax(), x, 10)
preprocess_test(ts_gminmax_diff(), x, 10)
preprocess_test(ts_swminmax(), x, 10)
preprocess_test(ts_anminmax(), x, 10)
preprocess_test(ts_animinmax(), x, 10)
preprocess_test(ts_aneminmax(), x, 10)
