#!/usr/bin/env Rscript
# Detect and remove unused references for a single main .tex file.

source(file.path("..", "ref_utils.R"))

main_tex  <- "path/to/main.tex"            # edit me
bib_file  <- "path/to/references.bib"      # edit me

refs <- unusedRef(main_tex, bib_file)
print(refs)
removeUnused(bib_file, refs)


