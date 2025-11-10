---
title: "Map and Replace Reference Keys (Directory)"
date: "2025-11-10"
output: html_document
---

Overview

Map old→new BibTeX keys between two `.bib` files and apply replacements to all `.tex` files under a directory.

Setup

```r
knitr::opts_chunk$set(message = FALSE, warning = FALSE)
source("https://raw.githubusercontent.com/eogasawara/mylibrary/refs/heads/main/references/ref_utils.R")
```

Inputs

```r
bib_old <- "path/to/references-old.bib"
bib_new <- "path/to/references.bib"
dir_tex <- "path/to/tex_dir"
```

Map and Replace

```r
mapRf <- mapRefs(bib_old, bib_new)
subMaps(dir_tex, mapRf)
cat("Applied key replacements in directory:", dir_tex, "\n")
```

