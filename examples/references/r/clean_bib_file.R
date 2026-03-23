knitr::opts_chunk$set(message = FALSE, warning = FALSE)
source("../../references/myReferences.R")
source("../../references/ref_utils.R")
source("reference_examples_setup.R")
ensure_reference_example_files()

bib_file <- "clean_references.bib" # arquivo .bib no diretorio atual

cleanBib(bib_file)
cat("Cleaning completed for:", bib_file, "\n")
