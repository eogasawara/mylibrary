# version 1.0
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/mySample.R")

data(iris)
head(iris)

# preparing dataset for random sampling
sr <- sample_random(iris)

# sampling dataset into train and test
sr <- train_test(sr)

# distribution of train
table(sr$train$Species)

# distribution of test
table(sr$test$Species)

# sampling dataset into four folds
sr <- k_fold(sr, 4)

# distribution of folds
tbl <- NULL
for (f in sr$folds) {
  tbl <- rbind(tbl, table(f$Species))
}
rownames(tbl) <- rep("random sampling", 4)
head(tbl)

# preparing dataset for random sampling
ss <- sample_stratified(iris, "Species")

# sampling dataset into train and test
ss <- train_test(ss)

# distribution of train
table(ss$train$Species)

# distribution of test
table(ss$test$Species)

# sampling dataset into four folds
ss <- k_fold(ss, 4)

# distribution of folds
tbl <- NULL
for (f in ss$folds) {
  tbl <- rbind(tbl, table(f$Species))
}
rownames(tbl) <- rep("random sampling", 4)
head(tbl)
