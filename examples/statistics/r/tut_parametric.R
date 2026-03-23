#dado sintético 
set.seed(1)
trials <- 30
MethodA <- rnorm(trials, mean=10, sd = 2)
MethodB <- rnorm(trials, mean=11, sd = 2)

data <- data.frame(MethodA, MethodB)
head(data)
boxplot(data)

shapiro.test(MethodA)

shapiro.test(MethodB)

library(nortest)

ad.test(MethodA)

ad.test(MethodB)

res <- t.test(MethodA, MethodB, paired=FALSE)
res

resL <- t.test(MethodA, MethodB, paired=FALSE, alternative = "less")
resL
resG <- t.test(MethodA, MethodB, paired=FALSE, alternative = "greater")
resG

res <- t.test(MethodA, MethodB, paired=TRUE)
res

resL <- t.test(MethodA, MethodB, paired=TRUE, alternative = "less")
resL
