## 03-kalman-filter-ar1.R
## Kalman filtering for an AR(1) time series fitted from yearly temperatures.

options(scipen = 999)

# Load yearly dataset (expects `temp_yearly` with `temperature` column)
yearly_path <- file.path("timeseries", "data", "noaa-global", "temp_yearly.RData")
if (!file.exists(yearly_path)) stop(paste("Dataset not found:", yearly_path))
load(yearly_path)

data <- temp_yearly$temperature
shift <- mean(data)
data <- data - shift
model <- arima(data, order=c(1, 0, 0))
ar_coef <- model$coef["ar1"]
error_sd <- sd(model$residuals)

if (FALSE) {
  shift <- 0
  set.seed(123)
  ar_coef <- 0.8
  error_sd <- 0.5
  data <- arima.sim(n = 100, list(ar = ar_coef), sd = error_sd)
}

# Define state space model
library(dlm)
library(ggplot2)

# State equation: x[t] = phi * x[t-1] + w[t]
FF <- matrix(ar_coef, ncol = 1)
GG <- matrix(1, nrow = 1)
V <- error_sd^2
m0 <- matrix(0, nrow = 1, ncol = 1)  # initial state
C0 <- matrix(1, nrow = 1, ncol = 1)  # initial covariance matrix
W <- matrix(V, nrow = 1, ncol = 1)  # system noise covariance matrix
model <- dlm(FF = FF, GG = GG, V = V, m0 = m0, C0 = C0, W = W)

# Apply Kalman Filter
filter_out <- dlmFilter(data, model)

# Extract filtered values
xhat <- filter_out$m[2:(length(data)+1)]

# Make predictions
num_preds <- 10
preds <- dlmForecast(filter_out, nAhead = num_preds)
xpred <- preds$m

data <- data + shift
xhat <- xhat + shift

# Plot results
df <- data.frame(
  time = 1:length(data),
  value = data,
  filtered = xhat
)

ggplot(df, aes(x = time)) +
  geom_line(aes(y = value), color = "black") +
  geom_line(aes(y = filtered), color = "red") +
  labs(
    title = "Kalman Filter for AR(1) Time Series",
    x = "Time",
    y = "Value"
  )
