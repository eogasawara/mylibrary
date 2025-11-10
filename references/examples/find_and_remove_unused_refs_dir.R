#!/usr/bin/env Rscript
# Detect and remove unused references across a directory of .tex files.

source("https://raw.githubusercontent.com/eogasawara/mylibrary/refs/heads/main/references/ref_utils.R")

dir_tex  <- "path/to/tex_dir"             # edit me
bib_file <- "path/to/references.bib"      # edit me

refs <- unusedRefs(dir_tex, bib_file)
print(refs)
removeUnused(bib_file, refs)


