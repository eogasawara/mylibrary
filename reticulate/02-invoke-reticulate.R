##
## 02-invoke-reticulate.R
## ----------------------
## Demonstrates interoperability between R and Python using reticulate.
## - Calls a simple Python function add(x, y)
## - Sends an R data.frame to Python, computes z = x + y, and gets it back
## - Saves a data.frame to RData, has Python update it, and loads it again

library(reticulate)
source_python('reticulate/01-python-bridge.py')

# 1) Sanity check: call a Python function
x <- add(5, 10)
print(sprintf("add(5, 10) = %s", x))

# 2) In-memory roundtrip: R -> Python (pandas) -> R
data <- data.frame(x = c(1:5), y = c(11:15))
dfm <- read_rdata_mem(data)
print(dfm)

# 3) File-based roundtrip via .RData
data <- data.frame(x = c(1:5), y = c(11:15))
filename <- file.path("reticulate", "table.RData")
save(data, file = filename, compress = TRUE)
read_rdata_file(filename)
load(file = filename)
print(data)
