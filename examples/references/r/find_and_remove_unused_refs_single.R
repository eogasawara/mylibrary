knitr::opts_chunk$set(message = FALSE, warning = FALSE)
source("../../references/myReferences.R")
source("../../references/ref_utils.R")
source("reference_examples_setup.R")
ensure_reference_example_files()

main_tex <- "unused_main.tex" # arquivo .tex no diretorio atual
bib_file <- "unused_single_references.bib" # arquivo .bib no diretorio atual

refs <- unusedRef(main_tex, bib_file)
print(refs)
removeUnused(bib_file, refs)
cat("Removed", length(refs), "unused references from:", bib_file, "\n")
