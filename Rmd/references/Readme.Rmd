Overview

This folder provides a small set of R utilities to manage bibliographies and LaTeX projects. The rendered examples below are available under `examples/references/`.

Capabilities

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

- [Reference Utilities Overview](../../examples/references/Readme.md) - Overview of the bibliography and LaTeX utilities available in this repository.
- [Build Query Strings from BibTeX](../../examples/references/build_query_strings.md) - Generate boolean search expressions from DOI fields or normalized titles.
- [Check BibTeX File for Issues](../../examples/references/check_bib_errors.md) - Validate entries and report missing fields or common inconsistencies.
- [Clean a Single BibTeX File](../../examples/references/clean_bib_file.md) - Normalize one bibliography file before reuse.
- [Clean All BibTeX Files in a Directory](../../examples/references/clean_bibs_dir.md) - Apply the cleaning workflow to every `.bib` file in a directory tree.
- [Expand LaTeX Includes](../../examples/references/expand_tex_includes.md) - Resolve `\input{}`, `\include{}`, `\import{}`, and related directives into a single expanded file.
- [Export Google Scholar Publications](../../examples/references/export_scholar_publications.md) - Collect an author's publications and export the result to a spreadsheet.
- [Find and Remove Unused References in a Directory](../../examples/references/find_and_remove_unused_refs_dir.md) - Detect unused citation keys across multiple `.tex` files and prune the bibliography.
- [Find and Remove Unused References in a Single File](../../examples/references/find_and_remove_unused_refs_single.md) - Detect unused citation keys for one main LaTeX document and prune the bibliography.
- [Map and Replace Reference Keys in a Directory](../../examples/references/map_ref_keys_and_replace_dir.md) - Update BibTeX keys and propagate the replacements across a directory of LaTeX files.
- [Map and Replace Reference Keys in a Single File](../../examples/references/map_ref_keys_and_replace_single.md) - Update BibTeX keys and rewrite citations in one LaTeX document.
- [Merge BibTeX Files Pairwise](../../examples/references/merge_bibs_pairwise.md) - Reconcile multiple `.bib` files through pairwise merges.
- [Print DOI URLs](../../examples/references/print_doi_urls.md) - Emit `https://doi.org/...` links for entries that contain DOI values.
- [Union of BibTeX Files](../../examples/references/union_bibs.md) - Build a consolidated bibliography from a directory of source files.

Reproducibility

- For fully reproducible runs without network, switch the source line in each example to the local path: `source("../ref_utils.R")` and ensure the required packages are installed.




