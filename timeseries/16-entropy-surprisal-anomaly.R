## 16-entropy-surprisal-anomaly.R
## Surprise score via histogram probabilities: s(x) = -log p(bin(x)).

set.seed(123)
normal_data <- rnorm(1000, mean = 10, sd = 1)
data <- normal_data
data[sample.int(length(data), 10)] <- rnorm(10, mean = 50, sd = 2)  # inject anomalies

# Discretize and compute empirical probabilities per bin
breaks <- pretty(range(data), n = 30)
h <- hist(data, breaks = breaks, plot = FALSE)
prob <- h$counts / sum(h$counts)

# Map each x to its bin probability and compute surprise
bin_index <- findInterval(data, vec = h$breaks, rightmost.closed = TRUE)
bin_index[bin_index == 0] <- 1
bin_index[bin_index > length(prob)] <- length(prob)
p_x <- prob[pmax(pmin(bin_index, length(prob)), 1)]
surprise <- -log(pmax(p_x, .Machine$double.eps))

# High surprise -> anomaly
thr <- quantile(surprise, 0.95)
is_anom <- surprise > thr

cat(sprintf("Detected %d anomalies (top 5%% surprise)\n", sum(is_anom)))

if (interactive()) {
  plot(data, pch = 19, col = ifelse(is_anom, "red", "blue"), main = "Entropy-inspired Surprise Anomalies")
}

