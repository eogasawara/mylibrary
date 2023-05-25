if (!exists("repos_name"))
  repos_name <<- getOption("repos")[1]

setrepos <- function(repos=repos) {
  repos_name <<- repos
}

loadlibrary <- function(packagename)
{
  if (!require(packagename, character.only = TRUE))
  {
    install.packages(packagename, repos=repos_name, dep=TRUE, verbose = FALSE)
    require(packagename, character.only = TRUE)
  }
}

loadlibrary("RefManageR")
loadlibrary("tibble")
loadlibrary("readxl")
loadlibrary("writexl")
loadlibrary("dplyr")
loadlibrary("stringr")

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

queryString <- function(bib, doi=TRUE) {
  bib <- ReadBib(bib, check = FALSE)
  bib_df <- as.data.frame(bib)
  bib_df <- rownames_to_column(bib_df)
  bib_df$title <- adjust_text(bib_df$title, lower=TRUE)
  str <- sprintf("TITLE(\"%s\")", bib_df$title)
  if (doi) {
    i <- !is.na(bib_df$doi)
    str[i] <- sprintf("DOI(\"%s\")", bib_df$doi[i])
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
    if (length(grep("backup", tex, ignore.case = TRUE)) == 0)
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
    if (length(grep(cod, tex, ignore.case = TRUE)) == 0)
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
    if (length(grep("backup", tex, ignore.case = TRUE)) == 0) {
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
  bib <- ReadBib(bibfile, check = FALSE)
  bib_df <- as.data.frame(bib)
  bib_df$abstract <- NA
  bib_df$keywords <- NA
  bib_df$url[bib$bibtype!="Misc"] <- NA
  bib_df$note <- NA
  bib_df$copyright <- NA
  if (doi)
    bib_df$doi <- NA
  bib <- as.BibEntry(bib_df)
  WriteBib(bib, bibfile)
}

cleanBibs <- function(dir, doi=FALSE) {
  bibs <- list.files(path = dir, pattern = ".bib$", full.names = TRUE, recursive = TRUE)
  
  for (bib in bibs) {
    if (length(grep("backup", bib, ignore.case = TRUE)) == 0) {
      cleanBib(bib, doi)
    }
  }
}


if (FALSE) {
  qry <- queryString('C:/Users/eduar/Downloads/Paper/references.bib', doi=TRUE)
  print(qry, quote = FALSE)
}

if (FALSE) {
  mapRf <- mapRefs('C:/Users/eduar/Downloads/Paper/references-org.bib', 'C:/Users/eduar/Downloads/Paper/references.bib')
  subMap('C:/Users/eduar/Downloads/Paper/main.tex', mapRf)
}

if (FALSE) {
  mapRf <- mapRefs('C:/Users/eduar/Downloads/Paper/references-org.bib', 'C:/Users/eduar/Downloads/Paper/references.bib')
  subMaps('C:/Users/eduar/Downloads/Paper', mapRf)
}


if (FALSE) {
  checkErrors('C:/Users/eduar/Downloads/Paper/references.bib')
}

if (FALSE) {
  cleanBib('C:/Users/eduar/Downloads/Paper/references.bib')
}

if (FALSE) {
  cleanBibs('C:/Users/eduar/Downloads/Paper')
}

if (FALSE) {
  refs <- unusedRef('C:/Users/eduar/Downloads/Paper/main.tex', 'C:/Users/eduar/Downloads/Paper/references.bib')
  removeUnused('C:/Users/eduar/Downloads/Paper/references.bib', refs)
}

if (FALSE) {
  refs <- unusedRefs('C:/Users/eduar/Downloads/Paper', 'C:/Users/eduar/Downloads/Paper/references.bib')
  removeUnused('C:/Users/eduar/Downloads/Paper/references.bib', refs)
}
