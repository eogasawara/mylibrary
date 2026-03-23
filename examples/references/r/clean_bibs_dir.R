knitr::opts_chunk$set(message = FALSE, warning = FALSE)
source("https://raw.githubusercontent.com/eogasawara/mylibrary/refs/heads/main/references/ref_utils.R")
# As duas linhas abaixo sao apenas para o cenario de teste desta documentacao.
# Em uso real, basta remover esse bloco e configurar corretamente as variaveis
# de entrada para apontarem para os seus arquivos reais.
source("https://raw.githubusercontent.com/eogasawara/mylibrary/refs/heads/main/references/reference_examples_setup.R")
download_reference_example_files()

dir_path <- "clean_dir" # diretorio com arquivos .bib no diretorio atual

cleanBibs(dir_path)
cat("Directory cleaned:", dir_path, "\n")
