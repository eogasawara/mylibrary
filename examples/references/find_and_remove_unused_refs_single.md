---
title: "Find and Remove Unused References (Single File)"
date: "2026-03-23"
output: html_document
---

Overview

Detect unused BibTeX references for one main LaTeX file and remove them from the `.bib` file.

Setup


``` r
knitr::opts_chunk$set(message = FALSE, warning = FALSE)
source("https://raw.githubusercontent.com/eogasawara/mylibrary/refs/heads/main/references/ref_utils.R")
source("https://raw.githubusercontent.com/eogasawara/mylibrary/refs/heads/main/references/reference_examples_setup.R")
download_reference_example_files()
```

Inputs


``` r
main_tex <- "unused_main.tex" # arquivo .tex no diretorio atual
bib_file <- "unused_single_references.bib" # arquivo .bib no diretorio atual
```

Find and Remove


``` r
refs <- unusedRef(main_tex, bib_file)
print(refs)
```

```
## [1] "unusedRef"
```

``` r
removeUnused(bib_file, refs)
cat("Removed", length(refs), "unused references from:", bib_file, "\n")
```

```
## Removed 1 unused references from: unused_single_references.bib
```

