---
title: "Map and Replace Reference Keys (Directory)"
date: "2026-03-23"
output: html_document
---

Overview

Map old→new BibTeX keys between two `.bib` files and apply replacements to all `.tex` files under a directory.

Setup


``` r
knitr::opts_chunk$set(message = FALSE, warning = FALSE)
source("https://raw.githubusercontent.com/eogasawara/mylibrary/refs/heads/main/references/ref_utils.R")
source("https://raw.githubusercontent.com/eogasawara/mylibrary/refs/heads/main/references/reference_examples_setup.R")
download_reference_example_files()
```

Inputs


``` r
bib_old <- "map_old_references.bib" # arquivo .bib antigo no diretorio atual
bib_new <- "map_new_references.bib" # arquivo .bib novo no diretorio atual
dir_tex <- "map_tex_dir" # diretorio com arquivos .tex no diretorio atual
```

Map and Replace


``` r
mapRf <- mapRefs(bib_old, bib_new)
subMaps(dir_tex, mapRf)
cat("Applied key replacements in directory:", dir_tex, "\n")
```

```
## Applied key replacements in directory: map_tex_dir
```

