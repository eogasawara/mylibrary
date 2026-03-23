knitr::opts_chunk$set(message = FALSE, warning = FALSE)
source("../../references/myReferences.R")
source("../../references/ref_utils.R")
source("reference_examples_setup.R")
ensure_reference_example_files()

bib_file <- "check_references.bib" # arquivo .bib no diretorio atual

checkErrors(bib_file)
