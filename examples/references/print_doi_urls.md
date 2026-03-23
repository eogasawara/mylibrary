---
title: "Print DOI URLs"
date: "2026-03-23"
output: html_document
---

Overview

Print `https://doi.org/<doi>` URLs for all entries in a `.bib` file that contain a DOI.

Setup


``` r
knitr::opts_chunk$set(message = FALSE, warning = FALSE)
source("https://raw.githubusercontent.com/eogasawara/mylibrary/refs/heads/main/references/ref_utils.R")
source("https://raw.githubusercontent.com/eogasawara/mylibrary/refs/heads/main/references/reference_examples_setup.R")
download_reference_example_files()
```

Inputs


``` r
bib_file <- "doi_references.bib" # arquivo .bib no diretorio atual
```

Print URLs


``` r
urlDOI(bib_file)
```

```
## https://doi.org/10.1000/sample-doi
```

```
## NULL
```

