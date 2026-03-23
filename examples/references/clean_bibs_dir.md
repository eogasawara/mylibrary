---
title: "Clean All BibTeX Files in a Directory"
date: "2026-03-23"
output: html_document
---

Overview

Scan a directory recursively and normalize/clean every `.bib` file found.

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
dir_path <- "clean_dir" # diretorio com arquivos .bib no diretorio atual
```

Clean


``` r
cleanBibs(dir_path)
cat("Directory cleaned:", dir_path, "\n")
```

```
## Directory cleaned: clean_dir
```

