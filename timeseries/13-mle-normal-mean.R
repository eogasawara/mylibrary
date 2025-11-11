## 13-mle-normal-mean.R
## MLE of the mean (mu) for a Normal distribution assuming known variance.

set.seed(1)
scores <- c(70, 75, 85, 90, 78)
sigma2 <- var(scores)  # treat as known for demo

loglik <- function(mu) {
  n <- length(scores)
  -n/2 * log(2 * pi * sigma2) - sum((scores - mu)^2) / (2 * sigma2)
}

# Maximize log-likelihood over a reasonable interval
opt <- optimize(loglik, interval = range(scores), maximum = TRUE)
mle_mu <- opt$maximum

cat(sprintf("MLE of mu: %.4f\n", mle_mu))

