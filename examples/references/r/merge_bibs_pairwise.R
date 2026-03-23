knitr::opts_chunk$set(message = FALSE, warning = FALSE)
source("../../references/myReferences.R")
source("../../references/ref_utils.R")
source("reference_examples_setup.R")
ensure_reference_example_files()

dir_path <- "merge_dir" # diretorio com arquivos .bib no diretorio atual

bibs <- list.files(path = dir_path, pattern = "\\.bib$", full.names = TRUE, recursive = TRUE)
if (length(bibs) >= 2) {
  for (i in 1:(length(bibs)-1)) {
    for (j in (i+1):length(bibs)) {
      join_Bib(bibs[i], bibs[j])
    }
  }
} else {
  message("Found fewer than two .bib files in ", dir_path)
}
