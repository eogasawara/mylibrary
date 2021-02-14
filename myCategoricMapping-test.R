# version 1.0
source("myCategoricMapping.R")

cm <- categ_mapping("Species")
iris_cm <- action(cm, iris)
print(head(iris_cm))


