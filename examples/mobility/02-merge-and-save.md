---
title: "Merge and save"
date: "2026-03-23"
output:
  html_document:
    toc: true
    toc_depth: 2
---

Overview

This R Markdown document preserves the original example script and adds enough context to help you study, adapt, and rerun the workflow safely.

02-merge-and-save.R Purpose ------- Load multiple Parquet parts for a given day, merge them, and save the combined result as an RData file. This script uses a small Python helper (via reticulate) to read Parquet files either from local disk (preferred when available) or from a remote URL as a fallback. Usage notes ----------- - Set the R working directory to the repository root so relative paths like 'mobility/...' resolve correctly. For interactive runs: setwd("path/to/repo/root") - Place Parquet files under 'mobility/data/'. If a file is not found locally, the script will attempt to fetch it from the configured URL. - The original workflow used merge() to combine parts. If your files are simple row-wise splits with identical schemas, consider using rbind() instead. We keep merge() here to preserve the original behavior.

How To Read

- Comece pelo resumo em `Overview` para entender o objetivo do exemplo antes de olhar o codigo.
- Revise caminhos de arquivos, pacotes e dependencias externas antes de adaptar o script ao seu ambiente.
- Use este documento como material de estudo: primeiro entenda o fluxo, depois ajuste entradas, credenciais e saidas para o seu caso.

Execution Notes

- O chunk principal foi mantido com `eval=FALSE` para que a documentacao possa ser convertida com seguranca, mesmo quando houver dependencias externas, APIs, Python auxiliar ou datasets locais indisponiveis.
- Para executar o exemplo de verdade, rode o codigo em uma sessao interativa ou remova `eval=FALSE` depois de conferir caminhos, pacotes e arquivos auxiliares.
- Mantenha os arquivos desta pasta juntos, porque varios exemplos dependem de recursos vizinhos como `.py`, `.csv`, `.xlsx`, `.RData`, imagens ou `README`.

Original Script

The chunk below reproduces the original script with minimal structural changes so the example remains faithful to the source material.


``` r
##
## 02-merge-and-save.R
##
## Purpose
## -------
## Load multiple Parquet parts for a given day, merge them, and save
## the combined result as an RData file. This script uses a small Python
## helper (via reticulate) to read Parquet files either from local disk
## (preferred when available) or from a remote URL as a fallback.
##
## Usage notes
## -----------
## - Set the R working directory to the repository root so relative paths
##   like 'mobility/...' resolve correctly. For interactive runs:
##     setwd("path/to/repo/root")
## - Place Parquet files under 'mobility/data/'. If a file is not found
##   locally, the script will attempt to fetch it from the configured URL.
## - The original workflow used merge() to combine parts. If your files are
##   simple row-wise splits with identical schemas, consider using rbind()
##   instead. We keep merge() here to preserve the original behavior.

library(reticulate)

# Load the Python helper with semantic numbering (relative to repo root)
source_python('mobility/01-load-parquet.py')

# -----------------------------
# Configuration
# -----------------------------
date_tag <- "2023-02-21"                 # dataset day
base_name <- paste0("G1-", date_tag)    # e.g., G1-2023-02-21
parts <- c("A", "B", "C", "D", "E")   # available parts to load
local_dir <- file.path("mobility", "data")
base_url <- "https://regulus.eic.cefet-rj.br/data/mobility"

# -----------------------------
# Load, merge, and save
# -----------------------------
data <- NULL

for (p in parts) {
  file_local <- file.path(local_dir, paste0(base_name, "-", p, ".parquet"))
  file_remote <- paste0(base_url, "/", base_name, "-", p, ".parquet")
  message("Loading part ", p, " ...")

  df_part <- tryCatch({
    if (file.exists(file_local)) {
      # Prefer local parquet if present
      read_parquet(file_local)
    } else {
      # Fallback to remote URL if local file does not exist
      read_parquet(file_remote)
    }
  }, error = function(e) {
    stop(paste("Failed to read part", p, ":", conditionMessage(e)))
  })

  if (is.null(data)) {
    data <- df_part
  } else {
    # Preserve original behavior: merge on common columns.
    # If you intend row-wise concatenation, replace with: data <- rbind(data, df_part)
    data <- merge(data, df_part)
  }
}

out_rdata <- paste0(base_name, ".RData")
save(data, file = file.path("mobility", out_rdata))
```




