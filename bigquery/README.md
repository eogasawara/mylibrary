# BigQuery

## Overview
- Retrieves census tracts for Campos dos Goytacazes (RJ) using `geobr`.
- Queries the most recent population from BigQuery via the `basedosdados` project.
- Computes simple estimates for households and trips and prints a summary table.

## Files
- `bigquery/BigQuery.R`: End-to-end R script that:
  - Loads required packages (and includes optional install commands).
  - Downloads census tract geometries for Campos dos Goytacazes.
  - Configures access to BigQuery via `basedosdados`.
  - Runs a SQL query to fetch the latest population for IBGE municipality `3301009`.
  - Calculates basic household and trip estimates and prints the result.

## Setup
- R 4.x recommended
- Packages: `geobr`, `sidrar`, `dplyr`, `sf`, `ggplot2`, `basedosdados`, `bigrquery`
- Google Cloud project for billing (`basedosdados::set_billing_id(...)`).
- BigQuery credentials available locally and exported via `BIGQUERY_TOKEN`.

## Authentication
- Set your billing project: `basedosdados::set_billing_id("<your-gcp-project-id>")`
- Point `BIGQUERY_TOKEN` to a JSON credentials file (service account or user token), e.g.:
  - macOS/Linux: `Sys.setenv(BIGQUERY_TOKEN = "~/my-creds.json")`
  - Windows: `Sys.setenv(BIGQUERY_TOKEN = "C:/Users/<you>/my-creds.json")`

## Usage
1. Install packages once if needed (inside the script or via `install.packages`).
2. Set your billing project and credentials as shown above.
3. Run the script:
   - From R: `source("bigquery/BigQuery.R")`
   - Or: `Rscript bigquery/BigQuery.R`

## Notes
- The script includes package installation lines for convenience; consider managing dependencies with `renv` for reproducibility.
- Accessing BigQuery may incur costs; ensure your billing project is appropriate.
- Update hardcoded paths/IDs to match your environment before running.
BigQuery Utilities
==================

Overview
- R helpers and notes for interacting with Google BigQuery.

Files
- [BigQuery.R](BigQuery.R) — R script to connect/query BigQuery.
- [README.md](README.md) — This document.
