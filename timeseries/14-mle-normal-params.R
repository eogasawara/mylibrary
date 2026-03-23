## 14-mle-normal-params.R
## MLE of both mean (mu) and standard deviation (sigma) for a Normal distribution.

set.seed(2)
x <- rnorm(500, mean = 0, sd = 1)

# Parameterization: pars = (mu, log_sigma) to enforce sigma > 0
NLL <- function(pars, data) {
  mu <- pars[1]
  log_sigma <- pars[2]
  sigma <- exp(log_sigma)
  -sum(dnorm(x = data, mean = mu, sd = sigma, log = TRUE))
}

fit <- optim(par = c(mu = 0, log_sigma = 0), fn = NLL, data = x)
mu_hat <- fit$par[1]
sigma_hat <- exp(fit$par[2])

cat(sprintf("MLE mu = %.4f, sigma = %.4f\n", mu_hat, sigma_hat))

