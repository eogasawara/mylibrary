#!/usr/bin/env Rscript
# Build a union .bib by gathering entries from a source directory.

source(file.path("..", "ref_utils.R"))

source_dir <- "path/to/source_dir"       # edit me
target_bib <- "path/to/union.bib"        # edit me

unionBibs(source_dir, target_bib)


