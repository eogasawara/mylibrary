# version 1.0
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/mySample.R")

data(iris)
head(iris)


process_samples <- function(sr, iris_data) {
  # sampling dataset into train and test
  tt <- train_test(sr, iris_data)
  
  # distribution of train
  print(table(tt$train$data$Species))
  
  # distribution of test
  print(table(tt$test$data$Species))
  
  # preparing dataset into four folds
  folds <- k_fold(sr, iris_data, 4)
  
  # distribution of folds
  tbl <- NULL
  for (f in folds) {
    tbl <- rbind(tbl, table(f$data$Species))
  }
  rownames(tbl) <- rep(class(sr)[1], 4)
  print(tbl)
}

# iris dataset
iris_data <- dal_data(iris)

#process_samples(sample_random(), iris_data)
process_samples(sample_stratified("Species"), iris_data)

