#!/usr/bin/env Rscript
# Build a union .bib by gathering entries from a source directory.

source("https://raw.githubusercontent.com/eogasawara/mylibrary/refs/heads/main/references/ref_utils.R")

source_dir <- "path/to/source_dir"       # edit me
target_bib <- "path/to/union.bib"        # edit me

unionBibs(source_dir, target_bib)


