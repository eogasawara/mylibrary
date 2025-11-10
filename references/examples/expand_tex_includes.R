#!/usr/bin/env Rscript
# Expand LaTeX includes: \input, \include, \import, \subimport
# Usage: set input/output paths below and source this file, or run with Rscript.

source("https://raw.githubusercontent.com/eogasawara/mylibrary/refs/heads/main/references/ref_utils.R")

# Edit these paths
input_tex  <- "path/to/main.tex"
output_tex <- "path/to/main_expanded.tex"  # or NULL to overwrite input

# Option A: write to a new file
expand_tex_includes(input_file = input_tex, output_file = output_tex)

# Option B: dry-run (returns expanded text in memory, does not write)
# expanded_text <- expand_tex_includes(input_file = input_tex, dry_run = TRUE)
# cat(substr(expanded_text, 1, 500), "...\n")


