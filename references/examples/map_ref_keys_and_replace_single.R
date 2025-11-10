#!/usr/bin/env Rscript
# Map old->new BibTeX keys and replace them in a single .tex file.

source("https://raw.githubusercontent.com/eogasawara/mylibrary/refs/heads/main/references/ref_utils.R")

bib_old   <- "path/to/references-old.bib"  # edit me
bib_new   <- "path/to/references.bib"      # edit me
tex_file  <- "path/to/main.tex"            # edit me

mapRf <- mapRefs(bib_old, bib_new)
subMap(tex_file, mapRf)


