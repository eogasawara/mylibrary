---
title: "Clean a Single BibTeX File"
date: "2025-11-10"
output: html_document
---

Overview

Normalize and clean one `.bib` file: fix common field issues, trim extra braces, and standardize casing where appropriate.

Setup

```r
knitr::opts_chunk$set(message = FALSE, warning = FALSE)
source("https://raw.githubusercontent.com/eogasawara/mylibrary/refs/heads/main/references/ref_utils.R")
```

Inputs

```r
# Edit this path to your .bib file
bib_file <- "path/to/references.bib"
```

Clean

```r
cleanBib(bib_file)
cat("Cleaning completed for:", bib_file, "\n")
```

