knitr::opts_chunk$set(message = FALSE, warning = FALSE)
source("https://raw.githubusercontent.com/eogasawara/mylibrary/refs/heads/main/references/ref_utils.R")
source("https://raw.githubusercontent.com/eogasawara/mylibrary/refs/heads/main/references/reference_examples_setup.R")
download_reference_example_files()

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
