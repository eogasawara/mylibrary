knitr::opts_chunk$set(message = FALSE, warning = FALSE)
source("../../references/myReferences.R")
source("../../references/ref_utils.R")
source("reference_examples_setup.R")
ensure_reference_example_files()

bib_file <- "query_references.bib" # arquivo .bib no diretorio atual

qry_doi   <- queryString(bib_file, doi = TRUE)
qry_title <- queryString(bib_file, doi = FALSE)

cat("\nQuery by DOI:\n");   print(qry_doi,   quote = FALSE)
cat("\nQuery by Title:\n"); print(qry_title, quote = FALSE)
