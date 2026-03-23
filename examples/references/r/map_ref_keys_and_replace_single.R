knitr::opts_chunk$set(message = FALSE, warning = FALSE)
source("https://raw.githubusercontent.com/eogasawara/mylibrary/refs/heads/main/references/ref_utils.R")
# As duas linhas abaixo sao apenas para o cenario de teste desta documentacao.
# Em uso real, basta remover esse bloco e configurar corretamente as variaveis
# de entrada para apontarem para os seus arquivos reais.
source("https://raw.githubusercontent.com/eogasawara/mylibrary/refs/heads/main/references/reference_examples_setup.R")
download_reference_example_files()

bib_old  <- "map_old_references.bib" # arquivo .bib antigo no diretorio atual
bib_new  <- "map_new_references.bib" # arquivo .bib novo no diretorio atual
tex_file <- "map_main.tex" # arquivo .tex no diretorio atual

mapRf <- mapRefs(bib_old, bib_new)
subMap(tex_file, mapRf)
cat("Applied key replacements to:", tex_file, "\n")
