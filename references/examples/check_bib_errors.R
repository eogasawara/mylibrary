#!/usr/bin/env Rscript
# Run checks on a .bib file and print issues.

source(file.path("..", "ref_utils.R"))

bib_file <- "path/to/references.bib"     # edit me
checkErrors(bib_file)


