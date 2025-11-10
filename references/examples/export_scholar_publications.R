#!/usr/bin/env Rscript
# Export publications from Google Scholar to an Excel file.

source(file.path("..", "ref_utils.R"))

first_name <- "FirstName"                 # edit me
last_name  <- "LastName"                  # edit me
out_xlsx   <- "path/to/articles.xlsx"     # edit me

get_scholar_citations(first_name, last_name, out_xlsx)


