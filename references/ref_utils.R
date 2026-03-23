# Reference Utilities (BibTeX/LaTeX/Scholar)
# This file provides helper functions to:
# - read/clean/merge BibTeX files and build queries
# - map old→new citation keys and substitute in .tex
# - expand LaTeX includes (input/include/import/subimport)
# - detect/remove unused references
# - export publications from Google Scholar
# Usage examples were extracted from inline if(FALSE) blocks and moved into
# separate scripts under `references/examples/` for clarity and reusability.
#
# ================================
# Utilitários para gerenciar .bib/.tex e citações
# - Limpeza e normalização de campos BibTeX
# - Geração de strings de consulta (por DOI ou título)
# - Mapeamento de chaves bibliográficas antigas -> novas
# - Substituições em arquivos .tex
# - Detecção e remoção de referências não utilizadas
# - Exportação de publicações do Google Scholar
# ================================

# ---- Pacotes usados (carregamento explícito) ----
library(RefManageR)  # leitura e manipulação de arquivos .bib (ReadBib, etc.)
library(tibble)      # utilitários de tibble (rownames_to_column)
library(readxl)      # leitura de planilhas Excel (não utilizado diretamente aqui)
library(writexl)     # escrita de planilhas Excel (write_xlsx)
library(dplyr)       # verbos de data wrangling (filter, select, etc.)
library(stringr)     # contagem/substituição com regex (str_count, str_replace)
library(scholar)     # consulta a perfis Google Scholar (get_scholar_id, get_publications)

# -----------------------------------------------
# Função: adjust_text
# Objetivo: remover chaves { } de um texto e, opcionalmente, convertê-lo para minúsculas.
# Parâmetros:
#   x     : vetor/char de entrada
#   lower : se TRUE, converte para minúsculas
# Retorno: character (mesmo conteúdo sem { } e possivelmente em minúsculas)
# Observação: útil para normalizar títulos vindos de BibTeX, que muitas vezes usam { } para preservar caixa.
# -----------------------------------------------
adjust_text <- function(x, lower=FALSE) {
  x <- gsub("\\{", "", x)
  x <- gsub("\\}", "", x)
  if (lower)
    x <- tolower(x)
  return(as.character(x))
}

# -----------------------------------------------
# Função: save_xls
# Objetivo: salvar um data frame/tibble em arquivo .xlsx.
# Parâmetros:
#   dataset  : data.frame/tibble a ser salvo
#   filename : caminho/arquivo de saída (.xlsx)
# Retorno: nenhum (efeito colateral: arquivo criado)
# -----------------------------------------------
save_xls <- function(dataset, filename) {
  write_xlsx(dataset, path = filename, col_names = TRUE)
}

# -----------------------------------------------
# Função: urlDOI
# Objetivo: imprimir (via cat) URLs https://doi.org/<doi> para cada entrada com DOI no .bib.
# Parâmetros:
#   bib : caminho do arquivo .bib
# Retorno: o mesmo vetor passado a cat (invisível); saída principal é impressa no console, 1 por linha.
# Observações:
#   - Remove registros sem DOI (NA).
#   - Normaliza o título (ajuste não afeta a URL; fica como efeito colateral no data frame local).
# -----------------------------------------------
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

# -----------------------------------------------
# Função: queryString
# Objetivo: gerar uma string de consulta para buscas (por ex., em bases externas),
#           preferindo DOI quando disponível.
# Parâmetros:
#   bib : caminho do arquivo .bib
#   doi : se TRUE, cria consultas no formato DOI("..."); se FALSE, usa TITLE("...")
# Retorno: o mesmo vetor passado a cat (invisível); saída principal é impressa (separada por " OR ").
# Regras:
#   - Se doi==TRUE: filtra entradas com DOI e gera DOI("...").
#   - Se doi==FALSE: se houver DOI, remove-as e usa TITLE("titulo normalizado").
# -----------------------------------------------
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

# -----------------------------------------------
# Função: mapRefs
# Objetivo: mapear chaves (rowname) entre dois .bib (antigo -> novo) com base no título normalizado.
# Parâmetros:
#   bib_old : caminho do .bib antigo
#   bib_new : caminho do .bib novo
# Retorno: data.frame com colunas 'from' (chave antiga) e 'to' (chave nova).
# Comportamento extra:
#   - Imprime chaves do arquivo antigo que não encontraram correspondência no novo (nmatch).
# Dependência: títulos precisam coincidir após normalização (minúsculas e sem { }).
# -----------------------------------------------
mapRefs <- function(bib_old, bib_new) {
  bib_old <- ReadBib(bib_old, check = FALSE)
  bib_old_df <- as.data.frame(bib_old)
  bib_old_df <- rownames_to_column(bib_old_df)
  bib_old_df$title <- adjust_text(bib_old_df$title, lower=TRUE)
  
  bib_new <- ReadBib(bib_new, check = FALSE)
  bib_new_df <- as.data.frame(bib_new)
  bib_new_df <- rownames_to_column(bib_new_df)
  bib_new_df$title <- adjust_text(bib_new_df$title, lower=TRUE)
  
  # Junção por título normalizado
  bib <- merge(bib_old_df, bib_new_df, by="title")
  
  # Identifica chaves antigas sem match
  nmatch <- bib_old_df |> filter(!(rowname %in% bib$rowname.x)) |> select(rowname, title)
  
  if (nrow(nmatch) > 0)
    print(nmatch)
  
  # Mantém apenas mapeamento 'from' (antigo) -> 'to' (novo)
  bib <- bib |> select(from=rowname.x, to=rowname.y)
  
  return(bib)
}

# -----------------------------------------------
# Função: subMap
# Objetivo: aplicar um mapeamento de chaves (from->to) dentro de um arquivo .tex.
# Parâmetros:
#   tex     : caminho do arquivo .tex a ser alterado
#   mapRefs : data.frame com colunas 'from' e 'to' (como retornado por mapRefs)
# Retorno: nenhum (efeito colateral: sobrescreve o .tex com substituições)
# Observações:
#   - Substitui TODAS as ocorrências de cada 'from' por 'to' via gsub.
# -----------------------------------------------
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

# -----------------------------------------------
# Função: subMaps
# Objetivo: aplicar subMap a todos os arquivos .tex dentro de um diretório (recursivo),
#           ignorando caminhos que contenham "backup".
# Parâmetros:
#   dir     : diretório raiz
#   mapRefs : data.frame com colunas 'from' e 'to'
# Retorno: nenhum (efeito colateral: sobrescreve arquivos .tex)
# -----------------------------------------------
subMaps <- function(dir, mapRefs) {
  texs <- list.files(path = dir, pattern = ".tex$", full.names = TRUE, recursive = TRUE)
  for (tex in texs) {
    if (length(grep("backup", tex, fixed = TRUE, ignore.case = TRUE)) == 0)
      subMap(tex, mapRefs)
  }
}

# -----------------------------------------------
# Função: unusedRef
# Objetivo: identificar chaves do .bib que NÃO aparecem em um arquivo .tex.
# Parâmetros:
#   tex : caminho do .tex
#   bib : caminho do .bib
# Retorno: vetor de chaves (rowname) não usadas no .tex.
# Observações:
#   - A busca por ocorrência é textual (grep), case-insensitive e literal.
# -----------------------------------------------
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

# -----------------------------------------------
# Função: unusedRefs
# Objetivo: identificar chaves do .bib não utilizadas em NENHUM .tex do diretório (recursivo),
#           ignorando arquivos/caminhos com "backup".
# Parâmetros:
#   dir : diretório com .tex
#   bib : caminho do .bib
# Retorno: vetor de chaves não usadas em todos os .tex.
# Lógica:
#   - Começa com "all" = todas as chaves do .bib.
#   - Para cada .tex, calcula unusedRef; mantém em "all" apenas as que continuam não usadas.
# -----------------------------------------------
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

# -----------------------------------------------
# Função: removeUnused
# Objetivo: reescrever um .bib removendo entradas cujas chaves constam em 'lst'.
# Parâmetros:
#   bib : caminho do arquivo .bib de entrada/saída (sobrescrito)
#   lst : vetor de chaves a remover
# Retorno: nenhum (efeito colateral: arquivo .bib sobrescrito sem as entradas removidas)
# Implementação:
#   - Varre linhas manualmente, acumulando cada entrada @type{key,...} até fechar chaves.
#   - Mantém balanço de chaves via contagem com stringr::str_count.
# -----------------------------------------------
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

# -----------------------------------------------
# Função: checkErrors
# Objetivo: depurar possíveis erros de parsing no .bib, salvando um arquivo parcial
#           até a linha i (quando há linha vazia) e tentando ler com ReadBib.
# Parâmetros:
#   bibfile : caminho do .bib
# Retorno: nenhum (efeito colateral: cria arquivo *_aux.bib; imprime índices de linhas vazias)
# Observação:
#   - Útil para identificar ponto de falha no parsing (por ex., entradas malformadas).
# -----------------------------------------------
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

# -----------------------------------------------
# Função: strip_inner_braces
# Objetivo: remover chaves internas de um texto preservando apenas o par mais externo.
# Parâmetros:
#   text : string de entrada
# Retorno: string sem chaves internas
# Uso: normalizar campos como 'title' que vêm com {Proteção de Caixa} em BibTeX.
# -----------------------------------------------
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

# -----------------------------------------------
# Função: sanitize_bib_field
# Objetivo: limpar/normalizar um campo específico nas entradas do .bib.
# Parâmetros:
#   lines : vetor de linhas do .bib
#   field : nome do campo a tratar (ex.: "title", "journal", "url", "doi", ...)
#   clean : se TRUE, remove o campo (ou, no caso de 'url', remove somente para tipos específicos)
# Retorno: vetor de linhas atualizado
# Regras:
#   - Quando clean==FALSE: remove chaves internas do valor e reescreve o campo como 'field = {valor_limpo},'
#   - Quando clean==TRUE:
#       * Para field == "url": só limpa (i.e., remove) se o tipo da entrada for 'inproceedings' ou 'article'
#       * Para os demais campos: remove o campo sempre
# Implementação:
#   - Faz parsing leve por balanço de chaves para capturar valores multilinha do campo-alvo.
# -----------------------------------------------
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
        # Remove o campo (não adiciona nada ao output)
        output <- output
      } else {
        # Mantém o campo, removendo apenas chaves internas
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

# -----------------------------------------------
# Função: cleanBib
# Objetivo: limpar um arquivo .bib, padronizando campos textuais e removendo campos desnecessários.
# Parâmetros:
#   bib : caminho do .bib a ser limpo (sobrescrito)
#   doi : se TRUE, também remove o campo 'doi'
# Retorno: nenhum (efeito colateral: sobrescreve o .bib)
# Regras:
#   - Normaliza {title, booktitle, journal, publisher} removendo chaves internas.
#   - Remove {abstract, keywords, note, copyright, url} (URL removida para 'article' e 'inproceedings').
#   - Se doi==TRUE, remove 'doi'.
# -----------------------------------------------
cleanBib <- function(bib, doi=FALSE) {
  lines <- readLines(bib, encoding = "UTF-8")
  for (campo in c("title", "booktitle", "journal", "publisher")) {
    lines <- sanitize_bib_field(lines, field = campo)
  }
  for (campo in c("abstract", "keywords", "note", "copyright", "url", "file")) {
    lines <- sanitize_bib_field(lines, field = campo, clean = TRUE)
  }
  if (doi) {
    for (campo in c("doi")) {
      lines <- sanitize_bib_field(lines, field = campo, clean = TRUE)
    }
  }
  writeLines(lines, bib, useBytes = TRUE)
}

# -----------------------------------------------
# Função: cleanBibs
# Objetivo: aplicar cleanBib a todos os .bib de um diretório (recursivo),
#           opcionalmente copiando-os antes para um diretório de saída.
# Parâmetros:
#   dir       : diretório raiz de busca
#   doi       : repassa o parâmetro 'doi' para cleanBib
#   diroutput : se não vazio, copia os .bib para lá e limpa a cópia
# Retorno: nenhum (efeito colateral: arquivos sobrescritos)
# Observação: ignora arquivos/pastas com "backup" no caminho.
# -----------------------------------------------
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

# -----------------------------------------------
# Função: get_scholar_citations
# Objetivo: obter publicações de um autor no Google Scholar e exportar para .xlsx.
# Parâmetros:
#   first    : primeiro nome
#   last     : sobrenome
#   filename : caminho do .xlsx de saída
# Retorno: nenhum (efeito colateral: arquivo .xlsx salvo)
# Observações:
#   - Usa scholar::get_scholar_id e scholar::get_publications.
#   - Requer que o pesquisador exista no Scholar e possa ser desambiguado.
# -----------------------------------------------
get_scholar_citations <- function(first, last, filename) {
  id <- get_scholar_id(first_name = first, last_name = last)
  p <- get_publications(id)
  write_xlsx(p, filename)
}

# ================================
# Expandir includes LaTeX in-place
# Suporta: \input{...}, \include{...}, \import{dir}{file}, \subimport{dir}{file}
# ================================

# Leitura segura em UTF-8 (retorna string única com quebras de linha)
.read_tex <- function(path) {
  lines <- readLines(file(path, encoding = "UTF-8"), warn = FALSE)
  paste(lines, collapse = "\n")
}

# Escrita segura em UTF-8
.write_tex <- function(text, path) {
  con <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(text, con, useBytes = TRUE)
}

# Normaliza caminho com base no diretório atual + extensão .tex se ausente
.resolve_path <- function(base_dir, rel, default_ext = ".tex") {
  rel <- trimws(rel)
  # remove aspas acidentais
  rel <- gsub('^"(.*)"$', "\\1", rel)
  rel <- gsub("^'(.*)'$", "\\1", rel)
  
  # Se caminho terminar com / ou não tiver extensão, adiciona .tex
  if (!grepl("\\.[A-Za-z0-9]+$", rel)) {
    # evita adicionar .tex após caminhos com barra final
    if (grepl("/$", rel)) rel <- sub("/$", "", rel)
    rel <- paste0(rel, default_ext)
  }
  # Caminho relativo ao base_dir
  full <- file.path(base_dir, rel)
  # Normaliza sem falhar caso não exista ainda (mustWork = FALSE)
  normalizePath(full, winslash = "/", mustWork = FALSE)
}

# Expansão recursiva de includes dentro de uma STRING de conteúdo
.expand_includes_text <- function(text, base_dir, visited) {
  # Padrões:
  # 1) \input{file} ou \include{file} (aceita [opcionais])
  pat_in <- "\\\\(input|include)\\s*(\\[[^\\]]*\\])?\\s*\\{([^}]+)\\}"
  # 2) \import{dir}{file} e \subimport{dir}{file}
  pat_imp <- "\\\\(import|subimport)\\s*\\{([^}]+)\\}\\s*\\{([^}]+)\\}"
  
  repeat {
    # Busca próximo match (o mais cedo) entre os dois padrões
    m_in  <- regexpr(pat_in,  text, perl = TRUE)
    m_imp <- regexpr(pat_imp, text, perl = TRUE)
    
    # Se nenhum encontrado, terminou
    if (m_in[1] == -1 && m_imp[1] == -1) break
    
    use_import <- FALSE
    if (m_in[1] == -1) use_import <- TRUE
    else if (m_imp[1] == -1) use_import <- FALSE
    else use_import <- (m_imp[1] < m_in[1])  # escolhe o que aparece primeiro
    
    if (!use_import) {
      # \input / \include
      start <- as.integer(m_in[1])
      len   <- attr(m_in, "match.length")
      end   <- start + len - 1
      
      # Capturas
      caps <- regmatches(text, m_in, invert = FALSE)
      # Extrai grupos nomeados pela regex
      # caps contém a substring inteira; vamos reexecutar com regexec para grupos:
      mm <- regexec(pat_in, caps, perl = TRUE)
      gg <- regmatches(caps, mm)[[1]]
      # gg[2] = comando (input/include)
      # gg[3] = opcional [..] (possível NA)
      # gg[4] = caminho
      rel_file <- gg[4]
      inc_path <- .resolve_path(base_dir, rel_file)
      
      # Evita loops de inclusão
      key <- normalizePath(inc_path, winslash = "/", mustWork = FALSE)
      if (key %in% visited) {
        replacement <- sprintf("%%%% [LaTeX-Expand] Skipped circular include: %s\n", inc_path)
      } else if (!file.exists(inc_path)) {
        replacement <- sprintf("%%%% [LaTeX-Expand] Missing file: %s\n", inc_path)
      } else {
        child_base <- dirname(inc_path)
        child_text <- .read_tex(inc_path)
        # Expande recursivamente o conteúdo incluído
        replacement <- .expand_includes_text(child_text, child_base, c(visited, key))
      }
      
      # Substitui no texto original
      pre  <- substr(text, 1, start - 1)
      post <- substr(text, end + 1, nchar(text))
      text <- paste0(pre, replacement, post)
      
    } else {
      # \import{dir}{file} / \subimport{dir}{file}
      start <- as.integer(m_imp[1])
      len   <- attr(m_imp, "match.length")
      end   <- start + len - 1
      
      caps <- regmatches(text, m_imp, invert = FALSE)
      mm <- regexec(pat_imp, caps, perl = TRUE)
      gg <- regmatches(caps, mm)[[1]]
      # gg[2] = comando (import/subimport)
      # gg[3] = dir
      # gg[4] = file
      rel_dir  <- gg[3]
      rel_file <- gg[4]
      
      # Base do import: base_dir + rel_dir
      import_base <- .resolve_path(base_dir, file.path(rel_dir, ""), default_ext = "")
      # .resolve_path pode adicionar extensão se faltar; forçamos diretório:
      import_base <- sub("[/\\\\]?$", "", import_base)
      
      inc_path <- .resolve_path(import_base, rel_file)
      
      key <- normalizePath(inc_path, winslash = "/", mustWork = FALSE)
      if (key %in% visited) {
        replacement <- sprintf("%%%% [LaTeX-Expand] Skipped circular import: %s\n", inc_path)
      } else if (!file.exists(inc_path)) {
        replacement <- sprintf("%%%% [LaTeX-Expand] Missing file: %s\n", inc_path)
      } else {
        child_base <- dirname(inc_path)
        child_text <- .read_tex(inc_path)
        replacement <- .expand_includes_text(child_text, child_base, c(visited, key))
      }
      
      pre  <- substr(text, 1, start - 1)
      post <- substr(text, end + 1, nchar(text))
      text <- paste0(pre, replacement, post)
    }
  }
  
  text
}

# --------------------------
# Função principal de usuário
# --------------------------
# expand_tex_includes(
#   input_file   : arquivo .tex principal
#   output_file   : opcional; se NULL, sobrescreve tex_path
#   dry_run    : se TRUE, não grava em disco; retorna o texto expandido
# )
expand_tex_includes <- function(input_file, output_file = NULL, dry_run = FALSE) {
  if (!file.exists(input_file)) stop("Arquivo .tex não encontrado: ", input_file)
  input_file <- normalizePath(input_file, winslash = "/", mustWork = TRUE)
  base_dir <- dirname(input_file)
  
  root <- .read_tex(input_file)
  expanded <- .expand_includes_text(
    root,
    base_dir,
    visited = normalizePath(input_file, winslash = "/", mustWork = TRUE)
  )
  
  if (isTRUE(dry_run)) return(expanded)
  
  if (is.null(output_file)) {
    output_file <- input_file
  }
  .write_tex(expanded, output_file)
  invisible(output_file)
}


# --------------------------
# Exemplo de uso:
# --------------------------

if (FALSE) {
  expanded_text <- expand_tex_includes("C:/Users/eduar/Downloads/Paper/main.tex", dry_run = TRUE)
  expand_tex_includes("C:/Users/eduar/Downloads/Paper/main.tex")                       # sobrescreve
  expand_tex_includes("C:/Users/eduar/Downloads/Paper/main.tex", "C:/Users/eduar/Downloads/Paper/main_full.tex")
}


# -----------------------------------------------
# Blocos de uso (desativados com if (FALSE)) — exemplos de chamadas
# - Mantidos como referência; ative trocando FALSE por TRUE e ajustando caminhos.
# -----------------------------------------------
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
  checkErrors("C:/Users/eduar/Downloads/Paper/references-org.bib")
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

if (FALSE) {
  # Expande todas as inclusões do LaTeX (input/include/import/subimport)
  # e salva em um novo arquivo sem sobrescrever o original
  expand_tex_includes(
    input_file  = "C:/Users/eduar/Downloads/Paper/main.tex",
    output_file = "C:/Users/eduar/Downloads/Paper/main_expanded.tex"
  )
  
  # Se quiser apenas ver o resultado expandido em memória (sem salvar):
  expanded_text <- expand_tex_includes(
    input_file = "C:/Users/eduar/Downloads/Paper/main.tex",
    dry_run    = TRUE
  )
  print(substr(expanded_text, 1, 500))  # mostra os primeiros 500 caracteres
}


