## 15-density-outlier-detection.R
## Kernel density estimate (KDE) based outlier detection: flag low-density points.

set.seed(123)
n <- 300
x <- c(rnorm(n, 0, 1), rnorm(5, 6, 0.5))  # include a few outliers

dens <- density(x, n = 2048)
# Interpolate KDE at observed points
px <- approx(dens$x, dens$y, xout = x, rule = 2)$y

# Flag as anomalies those below a low-density quantile
thr <- quantile(px, probs = 0.05)
is_anom <- px < thr

cat(sprintf("Detected %d anomalies out of %d points\n", sum(is_anom), length(x)))

if (interactive()) {
  plot(x, pch = 19, col = ifelse(is_anom, "red", "blue"), main = "KDE-based Outlier Detection")
}

