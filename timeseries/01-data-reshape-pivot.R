## 01-data-reshape-pivot.R
## Data wrangling examples: join yearly temperatures with oil metrics and pivot monthly data.

library(dplyr)
library(reshape)

# Load yearly temperatures
yearly_path <- file.path("timeseries", "data", "noaa-global", "temp_yearly.RData")
if (!file.exists(yearly_path)) stop(paste("Dataset not found:", yearly_path))
load(yearly_path)  # loads temp_yearly

# Load oil metrics (three columns repeated per period: production, demand, gap)
oil_path <- file.path("timeseries", "data", "oil", "data.csv")
if (!file.exists(oil_path)) stop(paste("Dataset not found:", oil_path))
oil <- read.csv(oil_path, header = FALSE)
oil <- as.vector(t(oil))

data <- temp_yearly[temp_yearly$x >= '1971-01-01' & temp_yearly$x <= '2020-01-01',]

data$production <- oil[(1:length(oil))%%3 == 1]
data$demand <- oil[(1:length(oil))%%3 == 2]
data$gap <- oil[(1:length(oil))%%3 == 0]
data$x <- format(data$x, "%Y")
data <- head(data |> dplyr::select(year = x, temperature, production), 10)


# Load monthly temperatures and pivot to wide (year x month)
monthly_path <- file.path("timeseries", "data", "noaa-global", "temp_monthly.RData")
if (!file.exists(monthly_path)) stop(paste("Dataset not found:", monthly_path))
load(monthly_path)  # loads temp_monthly
data_m <- temp_monthly[temp_monthly$x >= '1971-01-01' & temp_monthly$x <= '2020-12-01',]
data_m$year <- as.integer(format(data_m$x, "%Y"))
data_m$month <- as.integer(format(data_m$x, "%m"))
data_m$x <- NULL
data_m <- data_m |> dplyr::select(year, month, temperature)

result <- head(cast(data_m, year ~ month, fun=max), 10)
