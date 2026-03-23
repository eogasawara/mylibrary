knitr::opts_chunk$set(message = FALSE, warning = FALSE)
source("https://raw.githubusercontent.com/eogasawara/mylibrary/refs/heads/main/references/ref_utils.R")
source("https://raw.githubusercontent.com/eogasawara/mylibrary/refs/heads/main/references/reference_examples_setup.R")
download_reference_example_files()

first_name <- "Eduardo" # primeiro nome para busca no Google Scholar
last_name  <- "Ogasawara" # sobrenome para busca no Google Scholar
out_xlsx   <- "scholar_articles.xlsx" # arquivo .xlsx de saida no diretorio atual

if (identical(Sys.getenv("RUN_SCHOLAR_EXAMPLE"), "1")) {
  get_scholar_citations(first_name, last_name, out_xlsx)
  cat("Exported to:", out_xlsx, "\n")
} else {
  cat("Skipping network-dependent Google Scholar export. Set RUN_SCHOLAR_EXAMPLE=1 to execute.\n")
}
