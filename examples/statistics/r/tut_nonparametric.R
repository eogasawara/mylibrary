#dado sintético 
set.seed(1)
trials <- 30
MethodA <- rnorm(trials, mean=10, sd = 2)
MethodB <- rexp(trials, rate = 1/10)

data <- data.frame(MethodA, MethodB)
head(data)
boxplot(data)

shapiro.test(MethodA)

shapiro.test(MethodB)

library(nortest)

ad.test(MethodA)

ad.test(MethodB)

res <- wilcox.test(MethodA, MethodB, paired=FALSE, exact=FALSE)
res

res <- wilcox.test(MethodA, MethodB, paired=TRUE)
res
