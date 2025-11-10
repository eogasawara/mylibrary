---
title: "Print DOI URLs"
date: "2025-11-10"
output: html_document
---

Overview

Print `https://doi.org/<doi>` URLs for all entries in a `.bib` file that contain a DOI.

Setup

```r
knitr::opts_chunk$set(message = FALSE, warning = FALSE)
source("https://raw.githubusercontent.com/eogasawara/mylibrary/refs/heads/main/references/ref_utils.R")
```

Inputs

```r
bib_file <- "path/to/references.bib"
```

Print URLs

```r
urlDOI(bib_file)
```

