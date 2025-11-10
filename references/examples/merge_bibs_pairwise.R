#!/usr/bin/env Rscript
# Merge all .bib files pairwise inside a directory.

source("https://raw.githubusercontent.com/eogasawara/mylibrary/refs/heads/main/references/ref_utils.R")

dir_path <- "path/to/dir_with_bibs"     # edit me

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


