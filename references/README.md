References Utilities and Examples

This folder centralizes utilities and resources to manage BibTeX/LaTeX references and citation styles. The code is organized as a general-purpose library plus self-contained usage examples.

Structure
- `ref_utils.R` — Core R utilities for BibTeX, LaTeX include expansion, key mapping, cleaning, and Scholar export.
- `examples/` — One script per use case, ready to run after editing the input paths.
- `csl/` — Citation style files (`.csl`) for common styles.
- `tools/` — Auxiliary tools (e.g., a BibTeX translator script).

Citation Styles
- `csl/abnt.csl` — ABNT style.
- `csl/ieee.csl` — IEEE style.
- `csl/sbc.csl` — SBC (Brazilian Computing Society) style.

Tools
- `tools/bibtex.js` — BibTeX translator/utility script.

Use Cases
- `examples/expand_tex_includes.R` — Expand LaTeX `\input{}`, `\include{}`, `\import{}` and `\subimport{}` recursively and write a fully expanded `.tex` file.
- `examples/merge_bibs_pairwise.R` — Pairwise merge of all `.bib` files in a directory.
- `examples/build_query_strings.R` — Build boolean query strings from a `.bib` (prefer DOI or fallback to normalized title).
- `examples/print_doi_urls.R` — Print `https://doi.org/<doi>` URLs for all entries with DOI in a `.bib`.
- `examples/map_ref_keys_and_replace_single.R` — Map old→new BibTeX keys and replace them in a single `.tex` file.
- `examples/map_ref_keys_and_replace_dir.R` — Map old→new BibTeX keys and apply replacements to all `.tex` files in a directory.
- `examples/check_bib_errors.R` — Run basic BibTeX checks and report issues.
- `examples/clean_bib_file.R` — Normalize and clean a single `.bib` (field casing, braces, etc.).
- `examples/clean_bibs_dir.R` — Normalize and clean all `.bib` files under a directory.
- `examples/union_bibs.R` — Build a union `.bib` by gathering all entries from a source directory.
- `examples/find_and_remove_unused_refs_single.R` — Detect and remove unused references for a single main `.tex` file.
- `examples/find_and_remove_unused_refs_dir.R` — Detect and remove unused references across a directory of `.tex` files.
- `examples/export_scholar_publications.R` — Export publications from Google Scholar to an Excel file.

Notes
- Edit the paths at the top of each example script to match your files.
- All examples source `ref_utils.R`; make sure the required R packages are installed.

