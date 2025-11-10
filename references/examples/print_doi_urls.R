#!/usr/bin/env Rscript
# Print https://doi.org/<doi> for all entries with DOI.

source("https://raw.githubusercontent.com/eogasawara/mylibrary/refs/heads/main/references/ref_utils.R")

bib_file <- "path/to/references.bib"     # edit me
urlDOI(bib_file)


