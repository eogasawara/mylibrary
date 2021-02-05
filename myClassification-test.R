
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myPreprocessing.R")
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myPrediction.R")

load(url("https://github.com/eogasawara/mylibrary/raw/master/data/iris.RData"))
head(iris)

iris_tt = sample.random(iris)
iris_train = iris_tt[[1]]
iris_test = iris_tt[[2]]

tbl <- rbind(table(iris$Species), table(iris_train$Species), table(iris_test$Species))
rownames(tbl) <- c("dataset", "training", "test")
head(tbl)

model <- ZeroRule_model <- class_ZeroRule(iris_train, "Species")
head(model$train$metrics)

test <- ZeroRule_test <- class_test(model, iris_test, "Species")
head(test$predictions)

head(test$metrics)

head(test$conf_mat)

model <- tree_model <- class_tree(iris_train, "Species")
head(model$train$metrics)
test <- class_test(model, iris_test, "Species")
head(test$metrics)
head(test$conf_mat)

plot_size(5, 4)
plot(tree_model$model)
text(tree_model$model)

model <- nb_model <- class_naiveBayes(iris_train, "Species")
head(model$train$metrics)
test <- class_test(model, iris_test, "Species")
head(test$metrics)
head(test$conf_mat)

print(nb_model$model)

model <- rf_model <- class_randomForest(iris_train, "Species")
head(model$train$metrics)
test <- class_test(model, iris_test, "Species")
head(test$metrics)
head(test$conf_mat)

iris_train_n <- normalize.minmax(iris_train)
iris_train_n$data$Class <- iris_train$Class
iris_test_n  <- normalize.minmax(iris_test, iris_train_n$norm.set)
iris_test_n$data$Class <- iris_test$Class

model <- mlp_nnet_model <-  class_mlp_nnet(iris_train_n$data, "Species")
head(model$train$metrics)
test <- mlp_nnet_test <- class_test(model, iris_test_n$data, "Species")
head(test$metrics)
head(test$conf_mat)

model <- mlp_rsnns_model <- class_mlp_RSNNS(iris_train_n$data, "Species")
head(model$train$metrics)
test <- class_test(model, iris_test_n$data, "Species")
head(test$metrics)
head(test$conf_mat)

model <- rbf_rsnns_model <- class_rbf_RSNNS(iris_train_n$data, "Species")
head(model$train$metrics)
test <- class_test(model, iris_test_n$data, "Species")
head(test$metrics)
head(test$conf_mat)

model <- svm_rbf_model <- class_svm_rbf(iris_train_n$data, "Species")
head(model$train$metrics)
test <- class_test(model, iris_test_n$data, "Species")
head(test$metrics)
head(test$conf_mat)

model <- svm_poly_model <- class_svm_poly(iris_train_n$data, "Species")
head(model$train$metrics)
test <- class_test(model, iris_test_n$data, "Species")
head(test$metrics)
head(test$conf_mat)

model <- svm_sigmoid_model <-class_svm_sigmoid(iris_train_n$data, "Species")
head(model$train$metrics)
test <- class_test(model, iris_test_n$data, "Species")
head(test$metrics)
head(test$conf_mat)

model <- knn_model <-class_knn(iris_train_n$data, "Species", k=3)
head(model$train$metrics)
test <- class_test(model, iris_test_n$data, "Species")
head(test$metrics)
head(test$conf_mat)

plot_size(4, 3)
zr_rocr <- compute_rocr(ZeroRule_test$predictions, ZeroRule_test$values)
plot(zr_rocr)

mlp_nnet_rocr <- compute_rocr(mlp_nnet_test$predictions, ZeroRule_test$values)
plot(mlp_nnet_rocr)




