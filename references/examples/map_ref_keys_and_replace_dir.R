#!/usr/bin/env Rscript
# Map old->new BibTeX keys and apply replacements in all .tex files under a directory.

source("https://raw.githubusercontent.com/eogasawara/mylibrary/refs/heads/main/references/ref_utils.R")

bib_old <- "path/to/references-old.bib"  # edit me
bib_new <- "path/to/references.bib"      # edit me
dir_tex <- "path/to/tex_dir"             # edit me

mapRf <- mapRefs(bib_old, bib_new)
subMaps(dir_tex, mapRf)


