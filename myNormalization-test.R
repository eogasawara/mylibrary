# version 1.0
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myNormalization.R")

if (TRUE) {
  test_norm <- function(data, norm) {
    print(class(norm)[1])
    norm <- prepare(norm, data)
    ndata <- action(norm, data)
    print(head(ndata))
    
    ddata <- deaction(norm, ndata)
    print(head(ddata))
  }
  
  
  print(head(iris))
  
  test_norm(iris, minmax())
  
  test_norm(iris, zscore())
  
  test_norm(iris, zscore(nmean=0.5, nsd=0.5/2.698))
}

if (TRUE) {
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
  
  test_sw <- function(x, sw, norm) {
    ts <- ts_data(x, sw)
    print("org data")
    print(head(ts))
    
    samp <- ts_sample(ts)
    print("sample data")
    print(head(samp$train))
    
    norm <- prepare(norm, samp$train)
    
    proj <- ts_projection(samp$train)
    
    ninput <- action(norm, proj$input)
    print("normalized input")
    print(head(ninput))
    
    if (!is.null(proj$output)) {
      noutput <- action(norm, ninput, proj$output)
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
  
  if (TRUE) {
    test_sw(x, 0, ts_gminmax())
    test_sw(x, 10, ts_gminmax())
    test_sw(x, 0, ts_gminmax_diff())
    test_sw(x, 10, ts_gminmax_diff())
    test_sw(x, 10, ts_swminmax())
    test_sw(x, 10, ts_an())
  }
}