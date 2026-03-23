knitr::opts_chunk$set(message = FALSE, warning = FALSE)
source("https://raw.githubusercontent.com/eogasawara/mylibrary/refs/heads/main/references/ref_utils.R")
source("https://raw.githubusercontent.com/eogasawara/mylibrary/refs/heads/main/references/reference_examples_setup.R")
download_reference_example_files()

bib_file <- "clean_references.bib" # arquivo .bib no diretorio atual

cleanBib(bib_file)
cat("Cleaning completed for:", bib_file, "\n")
