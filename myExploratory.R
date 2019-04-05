source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myGraphics.R")

exp_table <- function (data, cnames = NULL, proj=NULL, n=6) {
  data <- data.frame(data)
  if (!is.null(proj))
    data = data[,-proj]
  if (!is.null(cnames))
    colnames(data) = cnames
  if (n > 0) {
    return(head(data, n))
  }
  return(data)
}

