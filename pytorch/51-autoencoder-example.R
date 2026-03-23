##
## 51-autoencoder-example.R
## ------------------------
## Demonstrates using the Python autoencoder helper on a synthetic
## time series with an injected anomaly.

library(daltoolbox)

data(sin_data)

sin_data$y[39] <- sin_data$y[39]*6  # inject a spike anomaly

sw_size <- 5
ts <- ts_data(sin_data$y, sw_size)

preproc <- ts_norm_gminmax()
preproc <- fit(preproc, ts)
ts <- transform(preproc, ts)

samp <- ts_sample(ts, test_size = 10)
train <- as.data.frame(samp$train)
test <- as.data.frame(samp$test)

library(reticulate)
source_python('pytorch/50-autoencoder.py')

ae <- autoencoder_create(input_size = 5, encoding_size = 3)
ae <- autoencoder_fit(ae, train)

result_enc <- autoencoder_encode(ae, test)
print(head(result_enc))

result_recon <- autoencoder_encode_decode(ae, test)
print(head(result_recon, 10))
