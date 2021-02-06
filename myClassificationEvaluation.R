# version 1.0
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myRelation.R")

classif_evaluation <- function(data, prediction) {
  obj <- atr_transform(data)
  obj$prediction <- prediction
  class(obj) <- append("classif_evaluation", class(obj))    
  return(obj)
}

prepare.classif_evaluation <- function(obj) {
  loadlibrary("RSNNS")  
  loadlibrary("nnet")  
  loadlibrary("MLmetrics")  
  adjust_predictions <- function(predictions) {
    predictions_i = as.matrix(Matrix(rep.int(0, nrow(predictions)*ncol(predictions)), nrow=nrow(predictions), ncol=ncol(predictions)))
    y <- apply(predictions, 1, which.is.max)
    for(i in unique(y)) {
      predictions_i[y==i,i] <- 1  
    }
    return(predictions_i)
  }
  predictions <- adjust_predictions(obj$prediction)
  values <- obj$data
  obj$conf_mat = RSNNS::confusionMatrix(values, predictions)
  obj$accuracy <- Accuracy(y_pred = predictions, y_true = values)
  obj$f1 <- F1_Score(y_pred = predictions, y_true = values, positive = 1)
  obj$sensitivity <- Sensitivity(y_pred = predictions, y_true = values, positive = 1)
  obj$specificity <- Specificity(y_pred = predictions, y_true = values, positive = 1)
  obj$precision <- Precision(y_pred = predictions, y_true = values, positive = 1)
  obj$recall <- Recall(y_pred = predictions, y_true = values, positive = 1)

  return(obj)
}

action.classif_evaluation <- function(obj) {
  metrics <- data.frame(accuracy=obj$accuracy, f1=obj$f1, sensitivity=obj$sensitivity, specificity=obj$specificity, precision=obj$precision, recall=obj$recall)
  return(metrics)
}

roc_curve <- function(obj) {
  UseMethod("roc_curve")
}

roc_curve.classif_evaluation <- function(obj) {
  loadlibrary("ROCR")
  pred <- prediction(obj$prediction, obj$data)
  rocr <- performance(pred, "tpr", "fpr")  
  return (rocr)  
}