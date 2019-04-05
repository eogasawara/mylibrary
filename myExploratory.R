source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myGraphics.R")
loadlibrary("reshape")

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

exp_norm_dist <- function(value, label_x = "std. normal", label_y = "",  color)  {
  data <- data.frame(value=value)
  grf <- ggplot(data, aes(sample = value)) +
    stat_qq(color=color) + theme_bw(base_size = 10) +
    stat_qq_line(color=color)
  grf <- grf + xlab(label_x)  
  grf <- grf + xlab(label_y)  
  return (grf)
}


