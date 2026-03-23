---
title: "Find and Remove Unused References (Directory)"
date: "2026-03-23"
output: html_document
---

Overview

Detect unused BibTeX references across all `.tex` files in a directory and remove them from the `.bib` file.

Setup


``` r
knitr::opts_chunk$set(message = FALSE, warning = FALSE)
source("https://raw.githubusercontent.com/eogasawara/mylibrary/refs/heads/main/references/ref_utils.R")
source("https://raw.githubusercontent.com/eogasawara/mylibrary/refs/heads/main/references/reference_examples_setup.R")
download_reference_example_files()
```

Inputs


``` r
dir_tex  <- "tex_dir" # diretorio com arquivos .tex no diretorio atual
bib_file <- "unused_dir_references.bib" # arquivo .bib no diretorio atual
```

Find and Remove


``` r
refs <- unusedRefs(dir_tex, bib_file)
print(refs)
```

```
## [1] "dirUnused"
```

``` r
removeUnused(bib_file, refs)
cat("Removed", length(refs), "unused references from:", bib_file, "\n")
```

```
## Removed 1 unused references from: unused_dir_references.bib
```

