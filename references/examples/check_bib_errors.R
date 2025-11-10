#!/usr/bin/env Rscript
# Run checks on a .bib file and print issues.

source("https://raw.githubusercontent.com/eogasawara/mylibrary/refs/heads/main/references/ref_utils.R")

bib_file <- "path/to/references.bib"     # edit me
checkErrors(bib_file)


