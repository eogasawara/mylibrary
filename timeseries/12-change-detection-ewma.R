## 12-change-detection-ewma.R
## EWMA-based change detection using a simple Harbinger-style interface.

source("https://raw.githubusercontent.com/eogasawara/TSED/refs/heads/main/code/header.R")

library(readxl)
library(daltoolbox)
library(dplyr)
library(stats)

hcp_ewma <- function(lambda = 0.2) {
  obj <- harbinger()
  obj$lambda <- lambda
  class(obj) <- append("hcp_ewma", class(obj))
  return(obj)
}

detect.hcp_ewma <- function(obj, serie, ...) {
  library(strucchange)
  if (is.null(serie)) stop("No data was provided for computation", call. = FALSE)

  non_na <- which(!is.na(serie))
  data <- serie[non_na]

  lambda <- obj$lambda  # Smoothing parameter
  n <- length(data)
  ewma <- numeric(n)
  ewma[1] <- data[1]
  for (i in 2:n) {
    ewma[i] <- lambda * data[i] + (1 - lambda) * ewma[i - 1]
  }
  x <- 1:n

  breaks <- breakpoints(y ~ x)

  inon_na <- rep(FALSE, length(non_na))
  nb <- length(breaks$breakpoints)
  if (nb > 0) inon_na[breaks$breakpoints] <- TRUE

  i <- rep(NA, length(serie))
  i[non_na] <- inon_na

  detection <- data.frame(idx = 1:length(serie), event = i, type = "")
  detection$type[i] <- "changepoint"
  return(detection)
}

set.seed(1)
n <- 100  # Number of time points
data <- c(sin((1:n)/pi), 2*sin((1:n)/pi), 10 + sin((1:n)/pi), 10-10/n*(1:n)+sin((1:n)/pi)/2, sin((1:n)/pi)/2)

model <- fit(hcp_ewma(), data)
detection <- detect(model, data)

grf <- har_plot(model, data, detection)
grf <- grf + ylab("value") + font

mypng(file = "figures/chap4_ewma.png", width = 1600, height = 720)
gridExtra::grid.arrange(grf, layout_matrix = matrix(c(1), byrow = TRUE, ncol = 2))
dev.off()

