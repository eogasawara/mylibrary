# version 1.0
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myNormalization.R")


test_norm <- function(data, norm) {
  print(class(norm)[1])
  norm$data <- data
  norm <- prepare(norm)
  norm <- action(norm)
  print(head(norm$data))
  
  norm <- deaction(norm)
  print(head(norm$data))
}


head(iris)

test_norm(iris, minmax())

test_norm(iris, zscore())

test_norm(iris, zscore(nmean=0.5, nsd=0.5/2.698))

