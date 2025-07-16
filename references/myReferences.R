library(RefManageR)
library(tibble)
library(readxl)
library(writexl)
library(dplyr)
library(stringr)
library(scholar)

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


queryString <- function(bib, doi=TRUE) {
  bib <- ReadBib(bib, check = FALSE)
  bib_df <- as.data.frame(bib)
  bib_df <- rownames_to_column(bib_df)
  bib_df$title <- adjust_text(bib_df$title, lower=TRUE)
  if (doi && !is.null(bib_df$doi)) {
    bib_df <- bib_df[!is.na(bib_df$doi),]
    str <- sprintf("DOI(\"%s\")", bib_df$doi)
  }
  else {
    if (!is.null(bib_df$doi))
      bib_df <- bib_df[is.na(bib_df$doi),]
    str <- sprintf("TITLE(\"%s\")", bib_df$title)
  }
  str <- cat(str, sep = "\n OR ")
  return(str)
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

removeUnused <- function(bib,lst) {
  lines <- readLines(bib, encoding = "UTF-8", warn = FALSE)
  output <- c()
  i <- 1
  
  while (i <= length(lines)) {
    line <- lines[i]
    
    if (grepl("^@", line)) {
      entry_start <- i
      entry_type <- sub("^@([a-zA-Z]+)\\{.*", "\\1", line)
      entry_key <- sub("^@[a-zA-Z]+\\{([^,]+),.*", "\\1", line)
      
      # Acumula a entrada até as chaves estarem balanceadas
      brace_level <- stringr::str_count(line, "\\{") - stringr::str_count(line, "\\}")
      i <- i + 1
      entry_lines <- line
      while (brace_level > 0 && i <= length(lines)) {
        entry_lines <- c(entry_lines, lines[i])
        brace_level <- brace_level + stringr::str_count(lines[i], "\\{") - stringr::str_count(lines[i], "\\}")
        i <- i + 1
      }
      
      if (!(entry_key %in% lst)) {
        output <- c(output, entry_lines, "")  # mantém a entrada
      }
    } else {
      # linha fora de entrada (provavelmente espaço)
      i <- i + 1
    }
  }
  
  writeLines(output, bib, useBytes = TRUE)
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

strip_inner_braces <- function(text) {
  chars <- strsplit(text, "")[[1]]
  output <- character()
  level <- 0
  for (char in chars) {
    if (char == "{") {
      level <- level + 1
      if (level == 1) output <- c(output, char)  # mantém chave externa
    } else if (char == "}") {
      if (level == 1) output <- c(output, char)  # mantém chave externa
      level <- level - 1
    } else {
      output <- c(output, char)
    }
  }
  paste(output, collapse = "")
}

sanitize_bib_field <- function(lines, field = "title", clean = FALSE) {
  output <- c()
  i <- 1
  field_pattern <- paste0("^\\s*", field, "\\s*=\\s*\\{")
  
  while (i <= length(lines)) {
    line <- lines[i]
    
    if (grepl(field_pattern, line)) {
      # Procurar tipo da entrada anterior (ex: @article{...})
      entry_type <- ""
      j <- i - 1
      while (j > 0) {
        if (grepl("^@\\w+", lines[j])) {
          entry_type <- tolower(sub("^@([a-zA-Z]+)\\{.*", "\\1", lines[j]))
          break
        }
        j <- j - 1
      }
      
      # Acumula linhas até fechar chaves corretamente
      full_line <- line
      brace_level <- stringr::str_count(line, "\\{") - stringr::str_count(line, "\\}")
      i <- i + 1
      while (brace_level > 0 && i <= length(lines)) {
        full_line <- paste0(full_line, lines[i])
        brace_level <- brace_level + stringr::str_count(lines[i], "\\{") - stringr::str_count(lines[i], "\\}")
        i <- i + 1
      }
      
      should_clean <- clean
      if (clean && field == "url") {
        # Só limpa URL se tipo for inproceedings ou article
        should_clean <- entry_type %in% c("inproceedings", "article")
      }
      
      if (should_clean) {
        output <- output
      } else {
        value <- sub(paste0("^.*", field, "\\s*=\\s*\\{"), "", full_line)
        value <- sub("\\},?\\s*$", "", value)
        value_clean <- strip_inner_braces(value)
        output <- c(output, sprintf("  %s = {%s},", field, value_clean))
      }
    } else {
      output <- c(output, line)
      i <- i + 1
    }
  }
  return(output)
}

cleanBib <- function(bib, doi=FALSE) {
  lines <- readLines(bib, encoding = "UTF-8")
  for (campo in c("title", "booktitle", "journal", "publisher")) {
    lines <- sanitize_bib_field(lines, field = campo)
  }
  for (campo in c("abstract", "keywords", "note", "copyright", "url")) {
    lines <- sanitize_bib_field(lines, field = campo, clean = TRUE)
  }
  if (doi) {
    for (campo in c("doi")) {
      lines <- sanitize_bib_field(lines, field = campo, clean = TRUE)
    }
  }
  writeLines(lines, bib, useBytes = TRUE)
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
  qry <- queryString("C:/Users/eduar/Downloads/Paper/references.bib", doi=TRUE)
  print(qry, quote = FALSE)
  
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
  print(refs)
  removeUnused("C:/Users/eduar/Downloads/Paper/references.bib", refs)
}

if (FALSE) {
  get_scholar_citations("Eduardo", "Ogasawara", "C:/Users/eduar/Downloads/articles.xlsx")
}

