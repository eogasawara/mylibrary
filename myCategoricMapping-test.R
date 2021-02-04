# version 1.0
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myCategoricMapping.R")


cm <- categ_mapping(iris, "Species")
iris.cm <- action(cm)
head(iris.cm)


