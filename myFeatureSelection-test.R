# version 1.0
source("myFeatureSelection.R")
data(iris)

select_features <- function(myfeature, data) {
  myfeature <- prepare(myfeature, data)
  print(myfeature$features)
  data <- action(myfeature, data)
  print(head(data))
}

select_features(feature_selection_lasso("Species"), iris)

select_features(feature_selection_fss("Species"), iris)

select_features(feature_selection_ig("Species"), iris)

select_features(feature_selection_relief("Species"), iris)


