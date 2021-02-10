# version 1.0
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myNormalization.R")


test_norm <- function(obj) {
  print(class(obj)[1])
  obj <- prepare(obj)
  obj$data <- action(obj)
  print(head(obj$data))
  
  obj$data <- deaction(obj)
  print(head(obj$data))
}


head(iris)

test_norm(minmax(iris))

test_norm(zscore(iris))

test_norm(zscore(iris, nmean=0.5, nsd=0.5/2.698))

