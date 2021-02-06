# version 1.0
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myRelation.R")

regression_evaluation <- function(data, prediction) {
  obj <- atr_transform(data)
  obj$prediction <- prediction
  class(obj) <- append("regression_evaluation", class(obj))    
  return(obj)
}

prepare.regression_evaluation <- function(obj) {
  #loadlibrary("RSNNS")  
  #loadlibrary("nnet")  
  #loadlibrary("MLmetrics")  
  values <- obj$data
  prediction <- obj$prediction
  obj$mse <- (sum(values - prediction)^2)/length(values)
  return(obj)
}

action.regression_evaluation <- function(obj) {
  metrics <- data.frame(mse=obj$mse)
  return(metrics)
}

