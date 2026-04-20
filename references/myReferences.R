if (!exists("repos_name"))
  repos_name <<- getOption("repos")[1]

setrepos <- function(repos=repos) {
  repos_name <<- repos
}

load_library <- function(packagename)
{
  if (!require(packagename, character.only = TRUE))
  {
    install.packages(packagename, repos=repos_name, dep=TRUE, verbose = FALSE)
    require(packagename, character.only = TRUE)
  }
}

load_library("RefManageR")
load_library("tibble")
load_library("readxl")
load_library("writexl")
load_library("dplyr")
load_library("stringr")
load_library("scholar")

adjust_text <- function(x, lower=FALSE) {
  x <- gsub("\\{", "", x)
  x <- gsub("\\}", "", x)
  if (lower)
    x <- tolower(x)
  return(as.character(x))
}

save_xls <- function(dataset, filename) {
  write_xlsx(dataset, path = filename, col_names = TRUE)
}

urlDOI <- function(bib) {
  bib <- ReadBib(bib, check = FALSE)
  bib_df <- as.data.frame(bib)
  bib_df <- rownames_to_column(bib_df)
  bib_df$title <- adjust_text(bib_df$title, lower=TRUE)
  str <- sprintf("https://doi.org/%s", bib_df$doi)
  str <- str[!is.na(bib_df$doi)]
  str <- cat(str, sep = "\n")
  return(str)
}


queryString <- function(bib, doi=TRUE, forceTitle=FALSE) {
  bib <- ReadBib(bib, check = FALSE)
  bib_df <- as.data.frame(bib)
  bib_df <- rownames_to_column(bib_df)
  bib_df$title <- adjust_text(bib_df$title, lower=TRUE)
  has_doi_column <- !is.null(bib_df$doi)

  if (forceTitle && has_doi_column) {
    bib_df <- bib_df[!is.na(bib_df$doi),]
    str <- sprintf("TITLE(\"%s\")", bib_df$title)
  }
  else if (doi && has_doi_column) {
    bib_df <- bib_df[!is.na(bib_df$doi),]
    str <- sprintf("DOI(\"%s\")", bib_df$doi)
  }
  else {
    if (has_doi_column)
      bib_df <- bib_df[is.na(bib_df$doi),]
    str <- sprintf("TITLE(\"%s\")", bib_df$title)
  }
  str <- paste(str, collapse = " OR ")
  cat(str, "\n", sep = "")
  return(invisible(str))
}

mapRefs <- function(bib_old, bib_new) {
  bib_old <- ReadBib(bib_old, check = FALSE)
  bib_old_df <- as.data.frame(bib_old)
  bib_old_df <- rownames_to_column(bib_old_df)
  bib_old_df$title <- adjust_text(bib_old_df$title, lower=TRUE)
  
  bib_new <- ReadBib(bib_new, check = FALSE)
  bib_new_df <- as.data.frame(bib_new)
  bib_new_df <- rownames_to_column(bib_new_df)
  bib_new_df$title <- adjust_text(bib_new_df$title, lower=TRUE)
  
  bib <- merge(bib_old_df, bib_new_df, by="title")
  
  nmatch <- bib_old_df |> filter(!(rowname %in% bib$rowname.x)) |> select(rowname, title)

  if (nrow(nmatch) > 0)
    print(nmatch)
  
  bib <- bib |> select(from=rowname.x, to=rowname.y)

  return(bib)
}

subMap <- function(tex, mapRefs) {
  texfile <- tex
  data <- readLines(con <- file(texfile, encoding = "UTF-8"))
  close(con)

  for (i in 1:nrow(mapRefs)) {
    from <- mapRefs$from[i]
    to <- mapRefs$to[i]
    data <- gsub(from, to, data)    
  }

  writeLines(data, con <- file(texfile, encoding = "UTF-8"))
  close(con)  
}

subMaps <- function(dir, mapRefs) {
  texs <- list.files(path = dir, pattern = ".tex$", full.names = TRUE, recursive = TRUE)
  for (tex in texs) {
    if (length(grep("backup", tex, fixed = TRUE, ignore.case = TRUE)) == 0)
      subMap(tex, mapRefs)
  }
}

unusedRef <- function(tex, bib) {
  tex <- readLines(con <- file(tex, encoding = "UTF-8"))
  close(con)
  lst <- NULL

  bib <- ReadBib(bib, check = FALSE)
  bib <- as.data.frame(bib)  
  bib <- rownames_to_column(bib)
  for (i in 1:nrow(bib)) {
    cod <- bib$rowname[i]
    if (length(grep(cod, tex, fixed = TRUE, ignore.case = TRUE)) == 0)
      lst <- append(lst, cod)
  }
  return(lst)  
}

unusedRefs <- function(dir, bib) {
  bibfile <- bib
  bib <- ReadBib(bibfile, check = FALSE)
  bib <- as.data.frame(bib)  
  bib <- rownames_to_column(bib)
  all <- bib$rowname
  
  texs <- list.files(path = dir, pattern = ".tex$", full.names = TRUE, recursive = TRUE)
  
  for (tex in texs) {
    if (length(grep("backup", tex, fixed = TRUE, ignore.case = TRUE)) == 0) {
      lst <- unusedRef(tex, bibfile)
      all <- all[(all %in% lst)] 
    }
  }
  return(all)  
}

removeUnused <- function(bib, lst) {
  bibfile <- bib
  bib <- ReadBib(bibfile, check = FALSE)
  bib_df <- as.data.frame(bib)
  
  x <- rownames(bib_df)
  
  bib_df <- bib_df[!(x %in% lst),]
  bib <- as.BibEntry(bib_df)
  WriteBib(bib, bibfile)
}

checkErrors <- function(bibfile) {
  x <- readLines(bibfile)  
  auxbibfile <- str_replace(bibfile, ".bib", "_aux.bib")
  for (i in 1:length(x)) {
    if ((i > 1) && (x[i] == "")) {
      print(i)
      fileConn<-file(auxbibfile)
      writeLines(x[1:i], fileConn)
      close(fileConn)    
      bib <- ReadBib(auxbibfile, check = FALSE)
    }
  }  
}


cleanBib <- function(bib, doi=FALSE) {
  bibfile <- bib
  lines <- readLines(con <- file(bibfile, encoding = "UTF-8"), warn = FALSE)
  close(con)

  field_is_complete <- function(field_lines) {
    text <- paste(field_lines, collapse = "\n")
    rhs <- sub("^[^=]*=\\s*", "", text)
    rhs <- trimws(rhs)

    braces <- lengths(regmatches(rhs, gregexpr("(?<!\\\\)\\{", rhs, perl = TRUE))) -
      lengths(regmatches(rhs, gregexpr("(?<!\\\\)\\}", rhs, perl = TRUE)))
    quotes <- lengths(regmatches(rhs, gregexpr("(?<!\\\\)\"", rhs, perl = TRUE)))

    return(braces <= 0 && quotes %% 2 == 0 && grepl(",\\s*$", rhs))
  }

  field_value <- function(field_lines) {
    text <- paste(field_lines, collapse = "\n")
    rhs <- sub("^[^=]*=\\s*", "", text)
    rhs <- sub(",\\s*$", "", trimws(rhs))
    rhs <- trimws(rhs)

    if (grepl("^\\{.*\\}$", rhs))
      rhs <- sub("^\\{(.*)\\}$", "\\1", rhs)
    else if (grepl("^\".*\"$", rhs))
      rhs <- sub("^\"(.*)\"$", "\\1", rhs)

    return(trimws(rhs))
  }

  removable_fields <- c("abstract", "keywords", "copyright", "note")
  cleaned <- character()
  field_buffer <- character()
  field_name <- NULL
  in_entry <- FALSE
  entry_header <- NULL
  entry_fields <- list()
  entry_footer <- NULL

  flush_entry <- function() {
    if (!in_entry)
      return()

    keep_idx <- rep(TRUE, length(entry_fields))
    field_names <- vapply(entry_fields, function(x) x$name, character(1))
    field_values <- vapply(entry_fields, function(x) field_value(x$lines), character(1))

    keep_idx[field_names %in% removable_fields] <- FALSE

    if (doi)
      keep_idx[field_names == "doi"] <- FALSE

    entry_has_doi <- any(field_names == "doi" & nzchar(field_values))
    has_volume <- any(field_names == "volume" & nzchar(field_values) & keep_idx)
    has_number <- any(field_names == "number" & nzchar(field_values) & keep_idx)

    if (entry_has_doi) {
      keep_idx[field_names == "url"] <- FALSE
      keep_idx[field_names == "urldate"] <- FALSE
    }

    if (has_volume && has_number)
      keep_idx[field_names == "number"] <- FALSE

    cleaned <<- c(cleaned, entry_header)
    for (idx in seq_along(entry_fields)) {
      if (keep_idx[idx])
        cleaned <<- c(cleaned, entry_fields[[idx]]$lines)
    }
    cleaned <<- c(cleaned, entry_footer)
  }

  flush_field <- function() {
    if (length(field_buffer) == 0)
      return()

    entry_fields[[length(entry_fields) + 1]] <<- list(
      name = field_name,
      lines = field_buffer
    )
  }

  for (line in lines) {
    if (!in_entry) {
      if (grepl("^\\s*@", line)) {
        in_entry <- TRUE
        entry_header <- line
        entry_fields <- list()
        entry_footer <- NULL
      } else {
        cleaned <- c(cleaned, line)
      }
      next
    }

    if (length(field_buffer) > 0) {
      field_buffer <- c(field_buffer, line)
      if (field_is_complete(field_buffer)) {
        flush_field()
        field_buffer <- character()
        field_name <- NULL
      }
      next
    }

    if (grepl("^\\s*}\\s*$", line)) {
      entry_footer <- line
      flush_entry()
      in_entry <- FALSE
      entry_header <- NULL
      entry_fields <- list()
      entry_footer <- NULL
      next
    }

    matches <- regexec("^\\s*([A-Za-z][A-Za-z0-9_-]*)\\s*=", line)
    parts <- regmatches(line, matches)[[1]]

    if (length(parts) > 1) {
      field_name <- tolower(parts[2])
      field_buffer <- line
      if (field_is_complete(field_buffer)) {
        flush_field()
        field_buffer <- character()
        field_name <- NULL
      }
    } else {
      entry_fields[[length(entry_fields) + 1]] <- list(
        name = NA_character_,
        lines = line
      )
    }
  }

  if (length(field_buffer) > 0)
    flush_field()

  if (in_entry && !is.null(entry_footer))
    flush_entry()

  writeLines(cleaned, con <- file(bibfile, encoding = "UTF-8"))
  close(con)
}

cleanBibs <- function(dir, doi=FALSE, diroutput = "") {
  bibs <- list.files(path = dir, pattern = ".bib$", full.names = TRUE, recursive = TRUE)
  if (diroutput != "") {
    file.copy(bibs, diroutput, overwrite = TRUE)    
    bibs <- list.files(path = diroutput, pattern = ".bib$", full.names = TRUE, recursive = TRUE)
  }
  
  for (bib in bibs) {
    if (length(grep("backup", bib, fixed = TRUE, ignore.case = TRUE)) == 0) {
      cleanBib(bib, doi)
    }
  }
}


unionBibs <- function(dir, filename) {
  bibs <- list.files(path = dir, pattern = ".bib$", full.names = TRUE, recursive = TRUE)
  
  all <- list()
  for (bibfile in bibs) {
    bib <- ReadBib(bibfile, check = FALSE)
    #bib_df <- as.data.frame(bib)
    #all <- rbind(all, bib)
    all <- append(all, bib)
  }
  #all <- as.BibEntry(all)
  WriteBib(all, filename)
}



join_Bib <- function(bibA, bibB) {
  bibA_df <- as.data.frame(ReadBib(bibA, check = FALSE))
  bibA_df$code <- rownames(bibA_df)
  bibB_df <- as.data.frame(ReadBib(bibB, check = FALSE))
  bibB_df$code <- rownames(bibB_df)
  bib_df <- merge(x = bibA_df, y = bibB_df, by = ("code"))
  if (nrow(bib_df) > 0) {
    print(sprintf("%s-%s", bibA, bibB))
    print(bib_df$code)
  }
}

get_scholar_citations <- function(first, last, filename) {
  id <- get_scholar_id(first_name = first, last_name = last)
  p <- get_publications(id)
  write_xlsx(p, filename)
}

if (FALSE) {
  dir <- "C:/Users/eduar/Downloads/Paper"
  bibs <- list.files(path = dir, pattern = ".bib$", full.names = TRUE, recursive = TRUE)
  
  for (i in 1:(length(bibs)-1)) {
    for (j in (i+1):length(bibs)) {
      join_Bib(bibs[i], bibs[j])
    }
  }
}

if (FALSE) {
  qry <- queryString("C:/Users/eduar/Downloads/Paper/references.bib", doi=FALSE)
  print(qry, quote = FALSE)
}

if (FALSE) {
  qry <- urlDOI("C:/Users/eduar/Downloads/Paper/references.bib")
  print(qry, quote = FALSE)
}

if (FALSE) {
  mapRf <- mapRefs("C:/Users/eduar/Downloads/Paper/references-org.bib", "C:/Users/eduar/Downloads/Paper/references.bib")
  subMap("C:/Users/eduar/Downloads/Paper/main.tex", mapRf)
}

if (FALSE) {
  mapRf <- mapRefs("C:/Users/eduar/Downloads/Paper/references-org.bib", "C:/Users/eduar/Downloads/Paper/references.bib")
  subMaps("C:/Users/eduar/Downloads/Paper", mapRf)
}


if (FALSE) {
  checkErrors("C:/Users/eduar/Downloads/Paper/references.bib")
}

if (FALSE) {
  cleanBib("C:/Users/eduar/Downloads/Paper/references.bib")
}

if (FALSE) {
  cleanBibs("C:/Users/eduar/Downloads/Paper")
}

if (FALSE) {
#  unionBibs("C:/Users/eduar/Downloads/Paper", "C:/Users/eduar/OneDrive - cefet-rj.br/book/all.bib")
  unionBibs("C:/Users/eduar/Downloads/Source", "C:/Users/eduar/Downloads/Paper/references.bib")
}


if (FALSE) {
  refs <- unusedRef("C:/Users/eduar/Downloads/Paper/main.tex", "C:/Users/eduar/Downloads/Paper/references.bib")
  print(refs)
  removeUnused("C:/Users/eduar/Downloads/Paper/references.bib", refs)
}

if (FALSE) {
  refs <- unusedRefs("C:/Users/eduar/Downloads/Paper", "C:/Users/eduar/Downloads/Paper/references.bib")
  removeUnused("C:/Users/eduar/Downloads/Paper/references.bib", refs)
}

if (FALSE) {
  get_scholar_citations("Eduardo", "Ogasawara", "C:/Users/eduar/Downloads/articles.xlsx")
}

if (FALSE) {
  #WriteBib(bib, file="C:/Users/eduar/Downloads/Paper/Books.bib")
  
  bibfile <- "C:/Users/eduar/Downloads/Paper/outliers.bib"
  refs <- unusedRefs("C:/Users/eduar/Downloads/Paper", bibfile)
  bib <- ReadBib(bibfile, check = FALSE)
  bibdf <- as.data.frame(bib)
  bibdf <- bibdf[rownames(bibdf) %in% refs,]
  bibdf$title <- gsub("\\{|\\}", "", bibdf$title)
  bibdf$id <- rownames(bibdf)
  bibdf <- bibdf |> select(id, title, doi)
  write_xlsx(bibdf, "C:/Users/eduar/Downloads/articles.xlsx")
}

