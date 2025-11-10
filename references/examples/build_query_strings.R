#!/usr/bin/env Rscript
# Build boolean query strings from a .bib.
# - DOI mode: DOI("...")
# - Title mode: TITLE("normalized title") when DOI is missing

source(file.path("..", "ref_utils.R"))

bib_file <- "path/to/references.bib"     # edit me

qry_doi   <- queryString(bib_file, doi = TRUE)
qry_title <- queryString(bib_file, doi = FALSE)

print(qry_doi,   quote = FALSE)
print(qry_title, quote = FALSE)


