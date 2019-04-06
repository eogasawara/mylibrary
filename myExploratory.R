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

exp_norm_dist <- function(value, label_x = "", label_y = "",  color)  {
  data <- data.frame(value = value)
  grf <- ggplot(data, aes(sample = value)) + 
    stat_qq(color=color) + xlab(label_x) + ylab(label_y) +
    theme_bw(base_size = 10) +
    stat_qq_line(color=color) 
  return (grf)
}


exp_correlation <- function(data, color) {
  cor_mat <- cor(data)
  cor_mat <- melt(cor_mat)
  colnames(cor_mat) <- c("x", "variable", "value")
  cor_mat$variable <- factor(cor_mat$variable, levels=sort(colnames(data)))
  cor_mat$x <- factor(cor_mat$x, levels=sort(colnames(data),decreasing=TRUE))
  cor_mat$radius <- abs(cor_mat$value)
  
  grf <- plot_ballon(cor_mat, color = color)   
  return(grf)
}


exp_pair_plot <- function(data, cnames, title='', clable= NULL, colors) {
  grf <- PairPlot(iris, cnames, title, group_var = clable, palette=NULL) + theme_bw(base_size = 10)
  if (is.null(clable)) 
    grf <- grf + geom_point(color=colors)
  else
    grf <- grf + scale_color_manual(values=colors) 
}
