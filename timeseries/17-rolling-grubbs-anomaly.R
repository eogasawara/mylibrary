## 17-rolling-grubbs-anomaly.R
## Rolling-window Grubbs' test to mark extreme outliers over time.

library(outliers)
library(zoo)

set.seed(7)
n <- 200
x <- rnorm(n)
x[sample.int(n, 3)] <- c(5, -6, 7)  # inject outliers

window_size <- 30
anomalies <- rep(FALSE, n)

for (i in 1:(n - window_size + 1)) {
  window <- x[i:(i + window_size - 1)]
  g <- grubbs.test(window)
  if (!is.na(g$p.value) && g$p.value < 0.05) {
    # Mark the most extreme point in this window
    idx_local <- which.max(abs(window - mean(window)))
    anomalies[i + idx_local - 1] <- TRUE
  }
}

cat(sprintf("Detected %d anomalous points\n", sum(anomalies)))

if (interactive()) {
  plot(x, pch = 19, col = ifelse(anomalies, "red", "black"), main = "Rolling Grubbs' Test")
}

