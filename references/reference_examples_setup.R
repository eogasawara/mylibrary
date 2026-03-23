download_reference_example_files <- function(base_dir = ".") {
  base_url <- "https://raw.githubusercontent.com/eogasawara/mylibrary/refs/heads/main/references/example_data"
  files <- c(
    "query_references.bib",
    "check_references.bib",
    "clean_references.bib",
    "clean_dir/a.bib",
    "clean_dir/b.bib",
    "expand_main.tex",
    "sections/intro.tex",
    "sections/details.tex",
    "unused_single_references.bib",
    "unused_main.tex",
    "unused_dir_references.bib",
    "tex_dir/chapter1.tex",
    "tex_dir/chapter2.tex",
    "map_old_references.bib",
    "map_new_references.bib",
    "map_main.tex",
    "map_tex_dir/part1.tex",
    "merge_dir/left.bib",
    "merge_dir/right.bib",
    "doi_references.bib",
    "union_source/one.bib",
    "union_source/two.bib"
  )
  
  for (rel in files) {
    dest <- file.path(base_dir, rel)
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    utils::download.file(
      url = sprintf("%s/%s", base_url, rel),
      destfile = dest,
      quiet = TRUE,
      mode = "wb"
    )
  }
}
