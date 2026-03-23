---
title: "Check BibTeX File for Issues"
date: "2026-03-23"
output: html_document
---

Overview

Run basic checks on a `.bib` file to identify missing fields, casing/braces problems, and other common issues.

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
bib_file <- "check_references.bib" # arquivo .bib no diretorio atual
```

Check


``` r
checkErrors(bib_file)
```

```
## [1] 8
```

