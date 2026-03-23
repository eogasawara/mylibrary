knitr::opts_chunk$set(message = FALSE, warning = FALSE)
source("../../references/myReferences.R")
source("../../references/ref_utils.R")
source("reference_examples_setup.R")
ensure_reference_example_files()

source_dir <- "union_source" # diretorio com arquivos .bib no diretorio atual
target_bib <- "union_output.bib" # arquivo .bib de saida no diretorio atual

unionBibs(source_dir, target_bib)
cat("Union written to:", target_bib, "\n")
