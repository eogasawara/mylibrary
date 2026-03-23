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

has_rstatix <- requireNamespace("rstatix", quietly = TRUE)
if (has_rstatix) {
  library(rstatix)
} else {
  message("Pacote 'rstatix' nao instalado; usando calculo alternativo de effect size.")
}

methods = c(rep("MethodA", length(MethodA)), rep("MethodB", length(MethodB)))
data_effect = data.frame(methods, y = c(MethodA, MethodB))

if (has_rstatix) {
  wilcox_effsize(y ~ methods, paired=TRUE, data=data_effect)
} else {
  diffs <- MethodA - MethodB
  diffs <- diffs[diffs != 0]
  ranks <- rank(abs(diffs))
  signed_rank_sum <- sum(sign(diffs) * ranks)
  rank_biserial <- signed_rank_sum / sum(ranks)
  data.frame(
    .y. = "y",
    group1 = "MethodA",
    group2 = "MethodB",
    effect_size = "rank_biserial",
    effsize = rank_biserial
  )
}
