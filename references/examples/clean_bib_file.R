#!/usr/bin/env Rscript
# Normalize/clean a single .bib file (fields, braces, casing).

source("https://raw.githubusercontent.com/eogasawara/mylibrary/refs/heads/main/references/ref_utils.R")

bib_file <- "path/to/references.bib"     # edit me
cleanBib(bib_file)


