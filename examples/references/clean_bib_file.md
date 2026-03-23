---
title: "Clean a Single BibTeX File"
date: "2026-03-23"
output: html_document
---

Overview

Normalize and clean one `.bib` file: fix common field issues, trim extra braces, and standardize casing where appropriate.

Setup


``` r
knitr::opts_chunk$set(message = FALSE, warning = FALSE)
source("../../references/myReferences.R")
source("../../references/ref_utils.R")
source("reference_examples_setup.R")
ensure_reference_example_files()
```

Inputs


``` r
bib_file <- "clean_references.bib" # arquivo .bib no diretorio atual
```

Clean


``` r
cleanBib(bib_file)
cat("Cleaning completed for:", bib_file, "\n")
```

```
## Cleaning completed for: clean_references.bib
```

