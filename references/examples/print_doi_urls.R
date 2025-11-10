#!/usr/bin/env Rscript
# Print https://doi.org/<doi> for all entries with DOI.

source(file.path("..", "ref_utils.R"))

bib_file <- "path/to/references.bib"     # edit me
urlDOI(bib_file)


