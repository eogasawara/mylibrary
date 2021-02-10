# version 1.0
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myBalance.R")

data(iris)
head(iris)
table(iris$Species)

myiris <- iris[c(1:20,51:100, 110:120),]
table(myiris$Species)

bo <- balance_oversampling(myiris, "Species")
bo <- balance(bo)

bs <- balance_subsampling(myiris, "Species")
bs <- balance(bs)

tbl <- rbind(table(myiris$Species), table(bo$data$Species), table(bs$data$Species))
rownames(tbl) <- c('unbalanced', 'oversampling', 'subsampling')
head(tbl)


