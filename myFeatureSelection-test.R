
#source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myPreprocessing.R")
data(iris)



select_features <- function(myfeature) {
  myfeature <- prepare(myfeature)
  print(myfeature$features)
  data <- action(myfeature)
  return(data)
}

select_features(feature_selection_lasso(iris, "Species"))

select_features(feature_selection_fss(iris, "Species"))

select_features(feature_selection_ig(iris, "Species"))


select_features(feature_selection_relief(iris, "Species"))


