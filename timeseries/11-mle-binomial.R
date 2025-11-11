## 11-mle-binomial.R
## Maximum Likelihood Estimation (MLE) for a binomial coin-flip probability p.
## Demonstrates likelihood, log-likelihood, and MLE using optimize.

set.seed(22)
n <- 100
heads <- rbinom(1, n, 0.5)  # observed number of heads

# Likelihood and log-likelihood for Binomial(n, p)
likelihood <- function(p) dbinom(heads, n, p)
loglik <- function(p) dbinom(heads, n, p, log = TRUE)

# Maximize log-likelihood over p in (0,1)
neg_loglik <- function(p) -loglik(p)
fit <- optimize(neg_loglik, interval = c(1e-6, 1 - 1e-6))
mle_p <- fit$minimum

cat(sprintf("Observed heads = %d out of n = %d\n", heads, n))
cat(sprintf("MLE for p = %.4f (observed proportion = %.4f)\n", mle_p, heads / n))

