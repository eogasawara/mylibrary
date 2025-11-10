---
title: "Merge BibTeX Files Pairwise"
date: "2025-11-10"
output: html_document
---

Overview

Enumerate all `.bib` files in a directory and call pairwise merges. Useful when reconciling entries across multiple bibliographies.

Setup

```r
knitr::opts_chunk$set(message = FALSE, warning = FALSE)
source("https://raw.githubusercontent.com/eogasawara/mylibrary/refs/heads/main/references/ref_utils.R")
```

Inputs

```r
dir_path <- "path/to/dir_with_bibs"
```

Merge Pairwise

```r
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
```

