## 05-multivariate-anomaly-pca.R
## Multivariate event detection using Harbinger (ARIMA baselines and PCA approach).

library(harbinger)
library(dplyr)

# Load multivariate example dataset
data(har_examples_multi)

#Using the time series 9
dataset <- har_examples_multi[[1]]
head(dataset)

#ploting series x and y
plot(x = 1:length(dataset$x), y = dataset$x)
lines(x = 1:length(dataset$x), y = dataset$x)

plot(x = 1:length(dataset$y), y = dataset$y)
lines(x = 1:length(dataset$y), y = dataset$y)

# Analysis: variable x isolated
model <- har_arima()
model <- fit(model, dataset$x)
detection <- detect(model, dataset$x)
print(detection |> dplyr::filter(event==TRUE))
evaluation <- evaluate(model, detection$event, dataset$event)
print(evaluation$confMatrix)

# first variable isolated did not identify event
grf <- plot.harbinger(model, dataset$x, detection, dataset$event)
plot(grf)

# Analysis: variable y isolated
model <- har_arima()
model <- fit(model, dataset$y)
detection <- detect(model, dataset$y)
print(detection |> dplyr::filter(event==TRUE))
evaluation <- evaluate(model, detection$event, dataset$event)
print(evaluation$confMatrix)


# second variable isolated did not identify event
grf <- plot.harbinger(model, dataset$y, detection, dataset$event)
plot(grf)


# Multivariate: PCA method 
model <- har_multi_pca()

# fitting the model using the two columns of the dataset
model <- fit(model, dataset[,1:2])

# making detections using har_multi_pca
detection <- detect(model, dataset[,1:2])

# filtering detected events
print(detection |> dplyr::filter(event==TRUE))

evaluation <- evaluate(model, detection$event, dataset$event)
print(evaluation$confMatrix)


# Plot the results (extract the merged serie from detection)
serie <- attr(detection, "serie")
grf <- plot.harbinger(model, serie, detection, dataset$event)
plot(grf)



