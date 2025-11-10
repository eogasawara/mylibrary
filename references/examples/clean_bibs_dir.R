#!/usr/bin/env Rscript
# Normalize/clean all .bib files under a directory.

source(file.path("..", "ref_utils.R"))

dir_path <- "path/to/dir_with_bibs"     # edit me
cleanBibs(dir_path)


