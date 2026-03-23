---
title: "Build Query Strings from BibTeX"
date: "2026-03-23"
output:
  html_document:
    toc: true
    toc_depth: 2
---

Overview

This example builds boolean query strings from a `.bib` file using two modes:

- DOI mode: `DOI("...")` for entries that have a DOI.
- Title mode: `TITLE("normalized title")` for entries without DOI (title is normalized).

Setup


``` r
knitr::opts_chunk$set(message = FALSE, warning = FALSE)
source("https://raw.githubusercontent.com/eogasawara/mylibrary/refs/heads/main/references/ref_utils.R")
```

```
## Warning: package 'RefManageR' was built under R version 4.5.1
```

```
## Warning: package 'tibble' was built under R version 4.5.2
```

```
## Warning: package 'readxl' was built under R version 4.5.1
```

```
## Warning: package 'writexl' was built under R version 4.5.1
```

```
## Warning: package 'dplyr' was built under R version 4.5.2
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```
## Warning: package 'stringr' was built under R version 4.5.2
```

```
## Warning: package 'scholar' was built under R version 4.5.1
```

``` r
# As duas linhas abaixo sao apenas para o cenario de teste desta documentacao.
# Em uso real, basta remover esse bloco e configurar corretamente as variaveis
# de entrada para apontarem para os seus arquivos reais.
source("https://raw.githubusercontent.com/eogasawara/mylibrary/refs/heads/main/references/reference_examples_setup.R")
download_reference_example_files()
```

Inputs


``` r
bib_file <- "query_references.bib" # arquivo .bib no diretorio atual
```

Build Queries


``` r
qry_doi   <- queryString(bib_file, doi = TRUE)
```

```
## DOI("10.1000/sample-doi")
```

``` r
qry_title <- queryString(bib_file, doi = FALSE)
```

```
## TITLE("a sample title only")
```

``` r
cat("\nQuery by DOI:\n");   print(qry_doi,   quote = FALSE)
```

```
## 
## Query by DOI:
```

```
## NULL
```

``` r
cat("\nQuery by Title:\n"); print(qry_title, quote = FALSE)
```

```
## 
## Query by Title:
```

```
## NULL
```

