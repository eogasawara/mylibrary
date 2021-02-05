source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myClassification.R")

source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/mySample.R")
data(iris)
head(iris)

# preparing dataset for random sampling
set.seed(1)
sr <- sample_random(iris)
sr <- train_test(sr)
iris_train = sr$train
iris_test = sr$test

loadlibrary("RSNNS")
iris_train_predictand = decodeClassLabels(iris_train[,"Species"])
iris_test_predictand = decodeClassLabels(iris_test[,"Species"])

tbl <- rbind(table(iris$Species), table(iris_train$Species), table(iris_test$Species))
rownames(tbl) <- c("dataset", "training", "test")
head(tbl)

model <- classification_zerorule(iris_train, "Species")
model <- prepare(model)
train_prediction <- action(model)

train_eval <- evaluation(iris_train_predictand, train_prediction)
train_eval <- prepare(train_eval)
res <- action(train_eval)
plot(roc_curve(train_eval))

#head(model$train$metrics)
#test <- ZeroRule_test <- class_test(model, iris_test, "Species")
#head(test$predictions)
