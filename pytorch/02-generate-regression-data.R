## 02-generate-regression-data.R
## Generate normalized Boston housing data and split into train/test.

library(daltoolbox)

data(Boston)
data <- Boston

preproc <- minmax()
preproc <- fit(preproc, data)
data <- transform(preproc, data)

sample <- sample_random()
tt <- train_test(sample, data)
train <- tt$train
test <- tt$test

library(reticulate)
source_python('pytorch/00-utils.py')

savedf(as.data.frame(train), 'pytorch/data/data_reg_train.csv')
savedf(as.data.frame(test), 'pytorch/data/data_reg_test.csv')

