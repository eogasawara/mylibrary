knitr::opts_chunk$set(message = FALSE, warning = FALSE)
source("../../references/myReferences.R")
source("../../references/ref_utils.R")
source("reference_examples_setup.R")
ensure_reference_example_files()

dir_path <- "clean_dir" # diretorio com arquivos .bib no diretorio atual

cleanBibs(dir_path)
cat("Directory cleaned:", dir_path, "\n")
