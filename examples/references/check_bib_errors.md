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
source("https://raw.githubusercontent.com/eogasawara/mylibrary/refs/heads/main/references/ref_utils.R")
# As duas linhas abaixo sao apenas para o cenario de teste desta documentacao.
# Em uso real, basta remover esse bloco e configurar corretamente as variaveis
# de entrada para apontarem para os seus arquivos reais.
source("https://raw.githubusercontent.com/eogasawara/mylibrary/refs/heads/main/references/reference_examples_setup.R")
download_reference_example_files()
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

