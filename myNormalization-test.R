# version 1.0
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myNormalization.R")


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

