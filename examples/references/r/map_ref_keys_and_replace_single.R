knitr::opts_chunk$set(message = FALSE, warning = FALSE)
source("../../references/myReferences.R")
source("../../references/ref_utils.R")
source("reference_examples_setup.R")
ensure_reference_example_files()

bib_old  <- "map_old_references.bib" # arquivo .bib antigo no diretorio atual
bib_new  <- "map_new_references.bib" # arquivo .bib novo no diretorio atual
tex_file <- "map_main.tex" # arquivo .tex no diretorio atual

mapRf <- mapRefs(bib_old, bib_new)
subMap(tex_file, mapRf)
cat("Applied key replacements to:", tex_file, "\n")
