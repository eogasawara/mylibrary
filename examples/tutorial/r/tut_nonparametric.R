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

has_nortest <- requireNamespace("nortest", quietly = TRUE)
if (!has_nortest) {
  message("Pacote 'nortest' nao instalado; teste Anderson-Darling sera omitido.")
}

if (has_nortest) {
  nortest::ad.test(MethodA)
} else {
  "Teste Anderson-Darling indisponivel sem o pacote 'nortest'."
}

if (has_nortest) {
  nortest::ad.test(MethodB)
} else {
  "Teste Anderson-Darling indisponivel sem o pacote 'nortest'."
}

res <- wilcox.test(MethodA, MethodB, paired=FALSE, exact=FALSE)
res

res <- wilcox.test(MethodA, MethodB, paired=TRUE)
res
