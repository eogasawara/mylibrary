source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myPreprocessing.R")
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myPrediction.R")

load(url("https://github.com/eogasawara/mylibrary/raw/master/data/wine.RData"))

wine_tt = sample.random(wine)
wine_train = wine_tt[[1]]
wine_test = wine_tt[[2]]

mynnet = class_mlp_nnet(wine_train, "X1")
head(mynnet$predictions)
mynnet$cmTrain
test <- class_test(mynnet$model, wine_test, "X1", predtype = mynnet$predtype)
head(test$predictions)
test$cmTest

mysvm_rbf = class_svm_rbf(wine_train, "X1")
head(mysvm_rbf$predictions)
mysvm_rbf$cmTrain
test <- class_test(mysvm_rbf$model, wine_test, "X1", predtype = mysvm_rbf$predtype)
head(test$predictions)
test$cmTest

mysvm_poly = class_svm_poly(wine_train, "X1")
head(mysvm_poly$predictions)
mysvm_poly$cmTrain
test <- class_test(mysvm_poly$model, wine_test, "X1", predtype = mysvm_poly$predtype)
head(test$predictions)
test$cmTest

mysvm_sig = class_svm_sigmoid(wine_train, "X1")
head(mysvm_sig$predictions)
mysvm_sig$cmTrain
test <- class_test(mysvm_sig$model, wine_test, "X1", predtype = mysvm_sig$predtype)
head(test$predictions)
test$cmTest

wine_train_n <- normalize.minmax(wine_train)
wine_train_n$data$X1 <- wine_train$X1
wine_test_n  <- normalize.minmax(wine_test, wine_train_n$norm.set)
wine_test_n$data$X1 <- wine_test$X1

myrsnns_mlp = class_mlp_RSNNS(wine_train_n$data, "X1")
head(myrsnns_mlp$predictions)
myrsnns_mlp$cmTrain
test <- class_test(myrsnns_mlp$model, wine_test_n$data, "X1", predtype = myrsnns_mlp$predtype)
head(test$predictions)
test$cmTest

myrsnns_rbf = class_rbf_RSNNS(wine_train_n$data, "X1")
head(myrsnns_rbf$predictions)
myrsnns_rbf$cmTrain
test <- class_test(myrsnns_rbf$model, wine_test_n$data, "X1", predtype = myrsnns_rbf$predtype)
head(test$predictions)
test$cmTest

mynb = class_naiveBayes(wine_train, "X1")
head(mynb$predictions)
mynb$cmTrain
test <- class_test(mynb$model, wine_test, "X1", predtype = mynb$predtype)
head(test$predictions)
test$cmTest

myrf = class_randomForest(wine_train, "X1")
head(myrf$predictions)
myrf$cmTrain
test <- class_test(myrf$model, wine_test, "X1", predtype = myrf$predtype)
head(test$predictions)
test$cmTest

myknn = knn_predict(wine_train, wine_test, "X1", k=3)
myknn$cmTest

myr0 = R0_predict(wine_train, wine_test, "X1")
myr0$cmTest

my.pred <- mynnet$predictions$X1
my.pred.class <- as.integer(mynnet$predictions$X1 > mynnet$predictions$X2 & mynnet$predictions$X1 > mynnet$predictions$X3)
my.true <- mynnet$predictions$X1.1

my.acc <- Accuracy(y_pred = my.pred.class, y_true = my.true)
my.f1 <- F1_Score(y_pred = my.pred.class, y_true = my.true, positive = "1")
my.sens <- Sensitivity(y_pred = my.pred.class, y_true = my.true, positive = "1")
my.spec <- Specificity(y_pred = my.pred.class, y_true = my.true, positive = "1")
my.prec <- Precision(y_pred = my.pred.class, y_true = my.true, positive = "1")
my.rec <- Recall(y_pred = my.pred.class, y_true = my.true, positive = "1")

my.roc.pred <- prediction(my.pred, my.true)
my.roc.perf <- performance(my.roc.pred, "tpr", "fpr")
options(repr.plot.width=4, repr.plot.height=4)
plot(my.roc.perf)
my.auc <- performance(my.roc.pred, "auc")@y.values[[1]]

