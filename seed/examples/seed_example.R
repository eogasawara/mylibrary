library(reticulate)

seed_file <- normalizePath(file.path("code", "seed.py"), winslash = "/", mustWork = TRUE)
python_bin <- Sys.which("python")

if (python_bin == "") {
  stop("Python interpreter not found in PATH.")
}

set.seed(123)
use_python(python_bin, required = TRUE)
source_python(seed_file)
seed_everything(123)
np <- import("numpy")

cat("R random:", paste(round(rnorm(3), 6), collapse = ", "), "\n")
cat("Python NumPy random:", paste(round(as.numeric(np$random$rand(3L)), 6), collapse = ", "), "\n")
