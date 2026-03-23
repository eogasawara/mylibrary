---
title: "Check BibTeX File for Issues"
date: "2026-03-23"
output: html_document
---

Overview

Run basic checks on a `.bib` file to identify missing fields, casing/braces problems, and other common issues.

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

Check

```r
checkErrors(bib_file)
```

