Reticulate Interoperability
===========================

Overview
- This folder contains a minimal, didactic experiment demonstrating how to call Python code from R using the reticulate package.
- The Python helper reads/writes R data frames (via reticulate and pyreadr) and performs simple transformations.

Goals
- Show a clear pattern for R↔Python roundtrips:
  - In-memory DataFrame exchange using reticulate
  - File-based exchange using `.RData` through pyreadr

Code
- Python bridge: `reticulate/01-python-bridge.py:1`
  - `add(x, y)`: connectivity sanity check.
  - `read_rdata_mem(df)`: reads a pandas DataFrame; adds column `z = x + y`.
  - `read_rdata_file(path)`: reads an `.RData` containing `data`, adds `z`, writes back.
- R invoker: `reticulate/02-invoke-reticulate.R:1`
  - Calls `add(5, 10)` and prints the result.
  - Builds a small data.frame, sends it to Python, receives back with `z`.
  - Saves `reticulate/table.RData`, asks Python to update it, then reloads and prints.

Data
- `reticulate/table.RData` is created by the invoker script at runtime.
- No external datasets are required.

How To Run
- From R: source `reticulate/02-invoke-reticulate.R:1`.
- Requirements:
  - R: reticulate
  - Python: pyreadr

Notes
- No Jupyter notebooks are present, so no conversions were needed.
- All comments and function docs are in English to keep things consistent with the rest of the repo.

Files
- [01-python-bridge.py](01-python-bridge.py) — Python helpers used from R via reticulate (add, data frame ops, RData I/O).
- [02-invoke-reticulate.R](02-invoke-reticulate.R) — Example R script demonstrating in-memory and file-based roundtrips.
- [table.RData](table.RData) — Example `.RData` file generated at runtime by the invoker.
