# version 1.0
source("mySample.R")

data(iris)
head(iris)


process_samples <- function(sr, iris) {
  # sampling dataset into train and test
  tt <- train_test(sr, iris)
  
  # distribution of train
  print(table(tt$train$Species))
  
  # distribution of test
  print(table(tt$test$Species))
  
  # preparing dataset into four folds
  folds <- k_fold(sr, iris, 4)
  
  # distribution of folds
  tbl <- NULL
  for (f in folds) {
    tbl <- rbind(tbl, table(f$Species))
  }
  rownames(tbl) <- rep(class(sr)[1], 4)
  print(tbl)
}

# iris dataset


process_samples(sample_random(), iris)
process_samples(sample_stratified("Species"), iris)

