knitr::opts_chunk$set(message = FALSE, warning = FALSE)
source("https://raw.githubusercontent.com/eogasawara/mylibrary/refs/heads/main/references/ref_utils.R")
# As duas linhas abaixo sao apenas para o cenario de teste desta documentacao.
# Em uso real, basta remover esse bloco e configurar corretamente as variaveis
# de entrada para apontarem para os seus arquivos reais.
source("https://raw.githubusercontent.com/eogasawara/mylibrary/refs/heads/main/references/reference_examples_setup.R")
download_reference_example_files()

bib_file <- "query_references.bib" # arquivo .bib no diretorio atual

qry_doi   <- queryString(bib_file, doi = TRUE)
qry_title <- queryString(bib_file, doi = FALSE)

cat("\nQuery by DOI:\n");   print(qry_doi,   quote = FALSE)
cat("\nQuery by Title:\n"); print(qry_title, quote = FALSE)
