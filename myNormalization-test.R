# version 1.0
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myNormalization.R")

mm <- minmax(iris)
mm <- prepare(mm)
iris.mm <- action(mm)


zs <- zscore(iris)
zs <- prepare(zs)
iris.zs <- action(zs)


zszo <- zscore(iris, nmean=0.5, nsd=0.5/2.698)
zszo <- prepare(zszo)
iris.zszo <- action(zszo)
