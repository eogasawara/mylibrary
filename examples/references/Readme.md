---
title: "Reference Utilities (BibTeX/LaTeX/Scholar)"
date: "2025-11-10"
output:
  html_document:
    toc: true
    toc_depth: 2
---

Overview

This folder provides a small set of R utilities to manage bibliographies and LaTeX projects:

- Read, clean, and merge BibTeX files.
- Build boolean query strings by DOI or title.
- Map old→new citation keys and substitute them in `.tex` files.
- Expand LaTeX includes (`\input{}`, `\include{}`, `\import{}`, `\subimport{}`).
- Detect and remove unused references.
- Export publications from Google Scholar.

Core Utility

- `ref_utils.R` (local): `references/ref_utils.R`
- Remote (pinned to main branch):
  - `source("https://raw.githubusercontent.com/eogasawara/mylibrary/refs/heads/main/references/ref_utils.R")`

Usage Notes

- The example notebooks below source the utilities from GitHub to avoid local path issues.
- Replace the placeholder paths in each example (e.g., `path/to/references.bib`) with your files.
- Required packages include `RefManageR`, `dplyr`, `stringr`, `writexl`, `readxl`, `scholar`, and base R dependencies.

Examples

- Build Query Strings: `examples/build_query_strings.Rmd` — Construct boolean queries from a `.bib` via DOI or normalized title.
- Check BibTeX Errors: `examples/check_bib_errors.Rmd` — Report common issues in a `.bib` file.
- Clean One BibTeX: `examples/clean_bib_file.Rmd` — Normalize and clean a single `.bib`.
- Clean Directory of BibTeX: `examples/clean_bibs_dir.Rmd` — Clean all `.bib` files recursively in a directory.
- Expand LaTeX Includes: `examples/expand_tex_includes.Rmd` — Expand LaTeX include/import directives to a single `.tex`.
- Export Scholar Publications: `examples/export_scholar_publications.Rmd` — Save Google Scholar publications to Excel.
- Find/Remove Unused (Single): `examples/find_and_remove_unused_refs_single.Rmd` — Detect and remove unused refs for one main `.tex`.
- Find/Remove Unused (Directory): `examples/find_and_remove_unused_refs_dir.Rmd` — Detect and remove unused refs across a directory of `.tex` files.
- Map & Replace Keys (Single): `examples/map_ref_keys_and_replace_single.Rmd` — Map old→new keys and update one `.tex` file.
- Map & Replace Keys (Directory): `examples/map_ref_keys_and_replace_dir.Rmd` — Map keys and update all `.tex` under a directory.
- Merge BibTeX Pairwise: `examples/merge_bibs_pairwise.Rmd` — Pairwise merge of all `.bib` files found in a folder.
- Print DOI URLs: `examples/print_doi_urls.Rmd` — Print `https://doi.org/<doi>` for `.bib` entries with DOI.
- Union of BibTeX Files: `examples/union_bibs.Rmd` — Build a union `.bib` from a source directory.

Reproducibility

- For fully reproducible runs without network, switch the source line in each example to the local path: `source("../ref_utils.R")` and ensure the required packages are installed.

