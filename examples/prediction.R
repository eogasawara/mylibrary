source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myPreprocessing.R")
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myPrediction.R")

load(url("https://github.com/eogasawara/mylibrary/raw/master/data/wine.RData"))

wine_tt = sample.random(wine)
wine_train = wine_tt[[1]]
wine_test = wine_tt[[2]]

wine_mlp_nnet = class_mlp_nnet(wine_train, wine_test, "X1")
wine_svm_rbf = class_svm_rbf(wine_train, wine_test, "X1")
wine_svm_poly = class_svm_poly(wine_train, wine_test, "X1")
wine_sigmoid_poly = class_svm_sigmoid(wine_train, wine_test, "X1")
wine_mlp_RSNNS = class_mlp_RSNNS(wine_train, wine_test, "X1")
wine_rbf_RSNNS = class_rbf_RSNNS(wine_train, wine_test, "X1")
wine_naiveBayes = class_naiveBayes(wine_train, wine_test, "X1")
wine_randomForest = class_randomForest(wine_train, wine_test, "X1")
wine_knn = class_knn(wine_train, wine_test, "X1", k=3)
wine_r0 = class_R0(wine_train, wine_test, "X1")

