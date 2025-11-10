#dado sintético 
set.seed(1)
trials <- 30
MethodA <- rnorm(trials, mean=10, sd = 2)
MethodB <- rnorm(trials, mean=11, sd = 2)

data <- data.frame(MethodA, MethodB)
head(data)
boxplot(data)

res <- wilcox.test(MethodA, MethodB, paired=TRUE, exact=FALSE)
res

library(rstatix)

methods = c(rep("MethodA", length(MethodA)), rep("MethodB", length(MethodB)))
data_effect = data.frame(methods, y = c(MethodA, MethodB))

wilcox_effsize(y ~ methods, paired=TRUE, data=data_effect)
