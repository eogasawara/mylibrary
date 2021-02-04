# version 1.0
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myPreprocessing.R")


cm <- categ_mapping(iris, "Species")
iris.cm <- action(cm)
head(iris.cm)


