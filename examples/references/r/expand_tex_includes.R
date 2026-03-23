knitr::opts_chunk$set(message = FALSE, warning = FALSE)
source("https://raw.githubusercontent.com/eogasawara/mylibrary/refs/heads/main/references/ref_utils.R")
source("https://raw.githubusercontent.com/eogasawara/mylibrary/refs/heads/main/references/reference_examples_setup.R")
download_reference_example_files()

input_tex  <- "expand_main.tex" # arquivo .tex no diretorio atual
output_tex <- "expand_main_expanded.tex"  # arquivo de saida no diretorio atual

expand_tex_includes(input_file = input_tex, output_file = output_tex)
cat("Expanded includes written to:", if (is.null(output_tex)) input_tex else output_tex, "\n")

# Only preview in memory (does not write to disk)
expanded_text <- expand_tex_includes(input_file = input_tex, dry_run = TRUE)
cat(substr(expanded_text, 1, 120), "...\n")
