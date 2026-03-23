Time Series Data
================

Place required datasets under this folder. Expected structure and filenames:

- noaa-global/
  - temp_monthly.RData — RData containing `temp_monthly` with columns `x` (Date) and `temperature`.
  - temp_yearly.RData — RData containing `temp_yearly` with columns `x` (Date) and `temperature`.
- oil/
  - data.csv — Wide CSV where each period contributes three values in order: production, demand, and gap. The script flattens this to vectors.

Notes
- Scripts in the parent folder reference files via `file.path("timeseries", "data", ...)` so they will work when run from the repo root.
- If your files have different names, adjust the paths in the scripts accordingly.
