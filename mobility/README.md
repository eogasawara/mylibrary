Mobility Experiments
====================

Overview
- This folder contains a small, didactic experiment to load Parquet parts from the Rio de Janeiro bus mobility dataset, merge them, and persist a combined artifact for subsequent analysis.
- The workflow is intentionally simple: a Python helper reads Parquet files, and an R script orchestrates loading, merging, and saving.

Context and Goals
- Article: Dataset on bus mobility and environmental indicators from Rio de Janeiro (Carvalho, D. et al., Scientific Data, 12(1):1569, Sep 26, 2025). DOI: 10.1038/s41597-025-05755-6.
- Goal: Provide a reproducible starting point to assemble daily Parquet parts (A–E) into a single object for downstream exploration.

Code Structure
- 01. Loader helper: `mobility/01-load-parquet.py:1` — Python function `read_parquet` that reads a local path or HTTP(S) URL.
- 02. Merge and save: `mobility/02-merge-and-save.R:1` — R script that:
  - Sources the Python helper via `reticulate`.
  - Prefers local Parquet files under `mobility/data/` and falls back to remote URLs if not found.
  - Merges parts A–E and saves `G1-YYYY-MM-DD.RData` in `mobility/`.

Data Location
- Local folder: `mobility/data/` (see `mobility/data/README.md:1` for expected filenames).
- If a part file is not present locally, the R script attempts to read the corresponding remote URL defined inside the script.

How To Run
- Prerequisites:
  - R with the `reticulate` package.
  - Python environment accessible to `reticulate` with `pandas` installed.
- Steps:
  - Set your R working directory to the repository root so relative paths resolve correctly.
  - Optionally place Parquet files under `mobility/data/` with the expected names.
  - Run `mobility/02-merge-and-save.R:1`.

Notes
- Combining parts: The script uses `merge()` to preserve the original behavior. If your Parquet parts are row-wise splits with identical schemas, consider replacing that step with `rbind()`.
- Additional reference: `mobility/documentation.txt:1` links to an external post about bus mobility.

Files
- [01-load-parquet.py](01-load-parquet.py) — Python helper to read Parquet (local or URL).
- [02-merge-and-save.R](02-merge-and-save.R) — Merge daily parts and save RData.
- [documentation.txt](documentation.txt) — External link reference.
- [data/README.md](data/README.md) — Expected Parquet part filenames and notes.
