## 04-generate-ts-classification-data.R
## Build time series supervised windows with event labels.

library(daltoolbox)
library(harbinger)

# Load example dataset and select a time series with events
data(har_examples)
dataset <- har_examples[[17]]

sw_size <- 4
ts <- ts_data(dataset$serie, sw_size)

preproc <- ts_norm_gminmax()
preproc <- fit(preproc, ts)
ts <- transform(preproc, ts)
ts <- as.data.frame(ts)
ts$event <- as.integer(dataset$event[sw_size:length(dataset$event)])

samp <- ts_sample(ts, test_size = 30)
train <- samp$train
test <- samp$test

library(reticulate)
source_python('pytorch/00-utils.py')

savedf(as.data.frame(train), 'pytorch/data/data_tscla_train.csv')
savedf(as.data.frame(test), 'pytorch/data/data_tscla_test.csv')

