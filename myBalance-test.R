# version 1.0
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myBalance.R")

data(iris)
head(iris)
table(iris$Species)

# iris dataset
iris_data <- dal_data(iris[c(1:20,51:100, 110:120),])
table(iris_data$data$Species)


teste_balance <- function(obj, obj_data)  {
  print(class(obj)[1])
  obj_data <- balance(obj, obj_data)
  print(table(obj_data$data$Species))
}

teste_balance(balance_oversampling("Species"), iris_data)
teste_balance(balance_subsampling("Species"), iris_data)
print(table(iris_data$data$Species))


