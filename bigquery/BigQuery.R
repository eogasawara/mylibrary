###############################################################################
# Purpose
#   Fetch census tracts for Campos dos Goytacazes (RJ), query the most recent
#   population from BigQuery via the basedosdados project, and compute simple
#   estimates for households and trips. Outputs a small tibble to the console.
#
# Requirements
#   - R packages: geobr, sidrar, dplyr, sf, ggplot2, basedosdados, bigrquery
#   - A Google Cloud project for billing with basedosdados
#   - A valid BigQuery credentials file and environment variable BIGQUERY_TOKEN
#
# Notes
#   - Package installations below are provided as a convenience. Prefer to
#     install them once beforehand or manage dependencies via renv.
#   - Update paths and project IDs to match your environment.
###############################################################################

# Packages — uncomment if you need to install them once
# install.packages(c("geobr", "sidrar", "dplyr", "sf", "ggplot2"))
# install.packages("basedosdados")
# install.packages("bigrquery")

# Load libraries
library(geobr)
library(sidrar)
library(dplyr)
library(sf)
library(ggplot2)
library(basedosdados)

# 1) Download census tracts for Campos dos Goytacazes (IBGE code: 3301009)
campos_setores <- read_census_tract(code_tract = 3301009, year = 2022)

# Optional: quick geometry preview
plot(campos_setores$geom)

# 2) Configure basedosdados/BigQuery access and query population data
#    - Replace with your GCP billing project ID
#    - Ensure BIGQUERY_TOKEN points to your credentials JSON (service account or
#      user token). Example path: "~/projeto-ibge-edu.json"
basedosdados::set_billing_id("projeto-ibge-edu")
Sys.setenv(BIGQUERY_TOKEN = "~/projeto-ibge-edu.json")

# Query the latest available population for Campos dos Goytacazes
pop_campos <- read_sql("
  SELECT
    ano,
    id_municipio,
    sigla_uf,
    populacao
  FROM `basedosdados.br_ibge_populacao.municipio`
  WHERE id_municipio = '3301009'
  ORDER BY ano DESC
  LIMIT 1
")

# Add municipality name for clarity (not provided by this table)
pop_campos$nome_municipio <- "Campos dos Goytacazes"

# 3) Estimate households and trips (simple illustrative coefficients)
pop <- pop_campos$populacao[1]
domicilios_est <- pop / 2.7
viagens_est <- 0.6 * pop + 1.4 * domicilios_est

# Assemble result table
resultado <- tibble(
  municipio = pop_campos$nome_municipio[1],
  ano = pop_campos$ano[1],
  populacao = pop,
  domicilios_est = round(domicilios_est),
  viagens_estimadas = round(viagens_est)
)

# Print result
print(resultado)

# Note: a leftover debug line `print(df_teste)` (undefined object) was removed
# to avoid runtime errors.
