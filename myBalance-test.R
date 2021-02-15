# version 1.0
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myBalance.R")

data(iris)
head(iris)
table(iris$Species)

# iris dataset
iris_data <- iris[c(1:20,51:100, 110:120),]
table(iris_data$Species)


teste_balance <- function(obj, data)  {
  print(class(obj)[1])
  data <- balance(obj, data)
  print(table(data$Species))
}


teste_balance(balance_oversampling("Species"), iris_data)
teste_balance(balance_subsampling("Species"), iris_data)



