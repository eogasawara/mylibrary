
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/mySample.R")

data(iris)
head(iris)
table(iris$Species)

myiris <- iris[c(1:20,51:100, 110:120),]
table(myiris$Species)

bo <- oversampling(myiris, "Species")
myiris.bo <- action(bo)

bs <- subsampling(myiris, "Species")
myiris.bs <- action(bs)

tbl <- rbind(table(myiris$Species), table(myiris.bo$Species), table(myiris.bs$Species))
rownames(tbl) <- c('unbalanced', 'oversampling', 'subsampling')
head(tbl)


