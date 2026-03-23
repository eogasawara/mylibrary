## 22-event-classification.R
## Build a sliding-window dataset with event labels from Harbinger examples.

source("https://raw.githubusercontent.com/cefet-rj-dal/harbinger-examples/main/jupyter.R")

# loading Harbinger
load_library("daltoolbox") 
load_library("harbinger")

# loading the example database
data(har_examples)

# Using the time series 17 
dataset <- harbinger::har_examples$example17

y <- dataset$serie
event <- as.integer(dataset$event)
data <- ts_data(y, 5)
data <- as.data.frame(data)

norm <- minmax()
norm <- fit(norm, data)
data <- transform(norm, data)

data$event <- event[5:length(event)]

train <- data[1:80,]
test <- data[81:109,]

dir.create("pytorch/data", showWarnings = FALSE, recursive = TRUE)
write.table(train, file="pytorch/data/event_train.csv", quote=FALSE, sep = ",", row.names = FALSE)
write.table(test, file="pytorch/data/event_test.csv", quote=FALSE, sep = ",", row.names = FALSE)
