Data directory for mobility experiments
======================================

Place the Parquet parts for a given day here. Expected filenames follow:

- G1-YYYY-MM-DD-A.parquet
- G1-YYYY-MM-DD-B.parquet
- G1-YYYY-MM-DD-C.parquet
- G1-YYYY-MM-DD-D.parquet
- G1-YYYY-MM-DD-E.parquet

Example for February 21, 2023:

- G1-2023-02-21-A.parquet
- G1-2023-02-21-B.parquet
- G1-2023-02-21-C.parquet
- G1-2023-02-21-D.parquet
- G1-2023-02-21-E.parquet

Notes
- Local files are preferred by the scripts. If a file is missing, the R script attempts a remote download using the base URL configured in `mobility/02-merge-and-save.R`.
- Keep files uncompressed and with the exact names so the loader can find them.
