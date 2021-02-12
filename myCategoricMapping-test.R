# version 1.0
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myCategoricMapping.R")

iris_data <- dal_data(iris)
cm <- categ_mapping("Species")
iris.cm <- action(cm, iris_data)
head(iris.cm$data)


