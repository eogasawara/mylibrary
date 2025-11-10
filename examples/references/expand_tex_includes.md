---
title: "Expand LaTeX Includes"
date: "2025-11-10"
output: html_document
---

Overview

Expand `\input{}`, `\include{}`, `\import{}` and `\subimport{}` directives in a main LaTeX file, optionally writing the fully expanded result to a new file.

Setup

```r
knitr::opts_chunk$set(message = FALSE, warning = FALSE)
source("https://raw.githubusercontent.com/eogasawara/mylibrary/refs/heads/main/references/ref_utils.R")
```

Inputs

```r
# Edit these paths
input_tex  <- "path/to/main.tex"
output_tex <- "path/to/main_expanded.tex"  # set NULL to overwrite input
```

Expand and Save

```r
expand_tex_includes(input_file = input_tex, output_file = output_tex)
cat("Expanded includes written to:", if (is.null(output_tex)) input_tex else output_tex, "\n")
```

Optional Dry Run

```r
# Only preview in memory (does not write to disk)
# expanded_text <- expand_tex_includes(input_file = input_tex, dry_run = TRUE)
# cat(substr(expanded_text, 1, 500), "...\n")
```

