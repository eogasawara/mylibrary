---
title: "Expand LaTeX Includes"
date: "2026-03-23"
output: html_document
---

Overview

Expand `\input{}`, `\include{}`, `\import{}` and `\subimport{}` directives in a main LaTeX file, optionally writing the fully expanded result to a new file.

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
input_tex  <- "expand_main.tex" # arquivo .tex no diretorio atual
output_tex <- "expand_main_expanded.tex"  # arquivo de saida no diretorio atual
```

Expand and Save


``` r
expand_tex_includes(input_file = input_tex, output_file = output_tex)
cat("Expanded includes written to:", if (is.null(output_tex)) input_tex else output_tex, "\n")
```

```
## Expanded includes written to: expand_main_expanded.tex
```

Optional Dry Run


``` r
# Only preview in memory (does not write to disk)
expanded_text <- expand_tex_includes(input_file = input_tex, dry_run = TRUE)
cat(substr(expanded_text, 1, 120), "...\n")
```

```
## \documentclass{article}
## \begin{document}
## Main text before include.
## This text comes from the intro section.
## This text com ...
```

