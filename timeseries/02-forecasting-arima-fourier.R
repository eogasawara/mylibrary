## 02-forecasting-arima-fourier.R
## Forecast a monthly global temperature series with ARIMA and ARIMA+Fourier.
## Expects an RData file containing `temp_monthly` with columns `x` (Date) and `temperature`.

library(forecast)

# Resolve dataset path relative to the repo root
monthly_path <- file.path("timeseries", "data", "noaa-global", "temp_monthly.RData")
if (!file.exists(monthly_path)) stop(paste("Dataset not found:", monthly_path))
load(monthly_path)  # loads temp_monthly

data <- temp_monthly
data$event <- FALSE
y <- data$temperature

# Create a monthly time series object starting at the first year in the data
start_year <- as.integer(format(min(data$x), "%Y"))
ts_data <- ts(y, frequency = 12, start = c(start_year, 1))

# Baseline ARIMA forecast
fit_arima <- auto.arima(ts_data)
fc_arima <- forecast(fit_arima, h = 24)
plot(fc_arima, main = "ARIMA")

# ARIMA with Fourier seasonal terms
K <- 6
fseason <- fourier(ts_data, K = K)
fit_fourier <- auto.arima(ts_data, xreg = fseason)
fforecast <- fourier(ts_data, K = K, h = 24)
fc_fourier <- forecast(fit_fourier, xreg = fforecast)
plot(fc_fourier, main = "ARIMA + Fourier")



