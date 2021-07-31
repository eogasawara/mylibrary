# version 1.2
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myTransform.R")
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myCategoricMapping.R")

iris <- datasets::iris
cm <- categ_mapping("Species")
iris_cm <- action(cm, iris)
print(head(iris_cm))


