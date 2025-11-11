## 04-event-detection-kmeans.R
## Univariate event detection using Harbinger's K-means method on example series.

library(harbinger)
library(dplyr)

# Load built-in example dataset
data(har_examples)

# Example A: time series 16
dataset <- har_examples[[16]]
plot(x = 1:length(dataset$serie), y = dataset$serie, main = "Series 16", xlab = "Time", ylab = "Value", type = "l")

model <- har_kmeans(k = 3)
model <- fit(model, dataset$serie)
detection <- detect(model, dataset$serie)
print(detection |> dplyr::filter(event == TRUE))

evaluation <- evaluate(model, detection$event, dataset$event)
print(evaluation$confMatrix)

grf <- plot.harbinger(model, dataset$serie, detection, dataset$event)
plot(grf)

# Example B: time series 1
dataset <- har_examples[[1]]
plot(x = 1:length(dataset$serie), y = dataset$serie, main = "Series 1", xlab = "Time", ylab = "Value", type = "l")

model <- har_kmeans(k = 1)
model <- fit(model, dataset$serie)
detection <- detect(model, dataset$serie)
print(detection |> dplyr::filter(event == TRUE))

evaluation <- evaluate(model, detection$event, dataset$event)
print(evaluation$confMatrix)

grf <- plot.harbinger(model, dataset$serie, detection, dataset$event)
plot(grf)
