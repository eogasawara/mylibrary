# mylibrary
General Description

This repository gathers reusable utilities, tutorials, and example notebooks for data analysis, statistics, bibliography/LaTeX workflows, and small supporting tools. Use the links below to explore rendered examples and additional documentation. Files under `Rmd/` contain source notebooks; this index focuses on Markdown (`.md`) and example outputs outside `Rmd/`.

Examples

- [Index of Example Notebooks](examples/Readme.md) - Complete listing of notebooks.

- R
  - [CRAN Downloads and Country Distribution](examples/r/package-usage.md) - Analyze CRAN downloads over time and inspect country distribution for a selected day.

- References (BibTeX/LaTeX/Scholar)
  - [Reference Utilities (BibTeX/LaTeX/Scholar)](examples/references/Readme.md) - Overview of utilities to manage bibliographies and LaTeX projects.
  - [Build Query Strings from BibTeX](examples/references/build_query_strings.md) - Construct boolean queries from a `.bib` by DOI or normalized title.
  - [Check BibTeX File for Issues](examples/references/check_bib_errors.md) - Identify missing fields and common issues in a `.bib` file.
  - [Clean a Single BibTeX File](examples/references/clean_bib_file.md) - Normalize and clean one `.bib` file.
  - [Clean All BibTeX Files in a Directory](examples/references/clean_bibs_dir.md) - Recursively clean every `.bib` in a directory.
  - [Expand LaTeX Includes](examples/references/expand_tex_includes.md) - Expand `\input{}`, `\include{}`, `\import{}`, and `\subimport{}` directives in a main LaTeX file.
  - [Export Google Scholar Publications](examples/references/export_scholar_publications.md) - Fetch an author's publications and export them to Excel.
  - [Find and Remove Unused References (Directory)](examples/references/find_and_remove_unused_refs_dir.md) - Detect unused references across a directory of `.tex` files and remove them from the `.bib`.
  - [Find and Remove Unused References (Single File)](examples/references/find_and_remove_unused_refs_single.md) - Detect unused references for one main `.tex` and remove them from the `.bib`.
  - [Map and Replace Reference Keys (Directory)](examples/references/map_ref_keys_and_replace_dir.md) - Map old–new BibTeX keys and update all `.tex` files in a directory.
  - [Map and Replace Reference Keys (Single .tex)](examples/references/map_ref_keys_and_replace_single.md) - Map old–new keys and update a single `.tex` file.
  - [Merge BibTeX Files Pairwise](examples/references/merge_bibs_pairwise.md) - Enumerate `.bib` files and perform pairwise merges to reconcile entries.
  - [Print DOI URLs](examples/references/print_doi_urls.md) - Print `https://doi.org/<doi>` URLs for `.bib` entries that contain a DOI.
  - [Union of BibTeX Files](examples/references/union_bibs.md) - Build a union `.bib` by collecting entries from a source directory.

- Statistics
  - [Regression](examples/statistics/linear_regression.md) - Hands-on linear, polynomial, multiple, and logistic regression with plots and examples.
  - [Effect Size Tutorial](examples/statistics/tut_effect_size.md) - Paired-sample comparison with Wilcoxon test and effect size computation.
  - [Nonparametric Tests Tutorial](examples/statistics/tut_nonparametric.md) - Normality checks (Shapiro-Wilk, Anderson-Darling) and Wilcoxon tests for independent and paired samples.
  - [Parametric Tests Tutorial](examples/statistics/tut_parametric.md) - Normality checks and t-tests (two-sample, paired, one-sided alternatives).

Additional Docs

- Datasets Overview: [data/README.md](data/README.md)
- References Folder Guide: [references/README.md](references/README.md)
- Subtitles Generator Guide: [subtitles/README.md](subtitles/README.md)
- [bigquery](bigquery) — R scripts and notes for Google BigQuery access.
- [pytorch](pytorch) — PyTorch experiments for tabular, time series, and graph data; standardized datasets under `pytorch/data`.
- [reticulate](reticulate) — R↔Python interoperability examples using reticulate and pyreadr.
- [stocks](stocks) — IBX50 dataset and scripts for basic stock data handling.
- [timeseries](timeseries) — Miscellaneous R time series scripts and examples.
- [tsfm](tsfm) — Tiny time series foundation model experiments and vanilla Transformer baseline.
- [mobility](mobility) — Bus mobility dataset loaders and merge scripts.
