## 01-generate-classification-data.R
## Prepare normalized Iris data with one-hot targets and split.

library(daltoolbox)

data(iris)
data <- iris

preproc <- minmax()
preproc <- fit(preproc, data)
data <- transform(preproc, data)

cm <- categ_mapping("Species")
data <- cbind(data, transform(cm, data))
data$Species <- NULL

sample <- sample_random()
tt <- train_test(sample, data)
train <- tt$train
test <- tt$test

library(reticulate)
source_python('pytorch/00-utils.py')

savedf(as.data.frame(train), 'pytorch/data/data_cla_train.csv')
savedf(as.data.frame(test), 'pytorch/data/data_cla_test.csv')

