# version 1.0
#source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/mySmoothing.R")


bi <- smoothing_inter(iris$Sepal.Length, n=2)
bi <- prepare(bi)
sl.bi <- action(bi)
table(sl.bi)

bf <- smoothing_freq(iris$Sepal.Length, n=2)
bf <- prepare(bf)
sl.bf <- action(bf)
table(sl.bf)

bc <- smoothing_cluster(iris$Sepal.Length, n=2)
bc <- prepare(bc)
sl.bc <- action(bc)
table(sl.bc)
