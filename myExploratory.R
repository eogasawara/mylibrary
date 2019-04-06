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


exp_pair_plot <- function(data, cnames, title = NULL, clabel = NULL, colors) {
  grf <- PairPlot(data, cnames, title, group_var = clabel, palette=NULL) + theme_bw(base_size = 10)
  if (is.null(clabel)) 
    grf <- grf + geom_point(color=colors)
  else
    grf <- grf + scale_color_manual(values=colors) 
  return (grf)
}

exp_advpair_plot <- function(data, cnames, title = NULL, clabel= NULL, colors) {
  if (!is.null(clabel)) {
    data$clabel <- data[,clabel]
    cnames <- c(cnames, 'clabel')
  }
  
  icol <- match(cnames, colnames(data))
  icol <- icol[!is.na(icol)]

  if (!is.null(clabel)) {
    grf <- ggpairs(data, columns = icol, aes(colour = clabel, alpha = 0.4)) + theme_bw(base_size = 10) 
    
    for(i in 1:grf$nrow) {
      for(j in 1:grf$ncol){
        grf[i,j] <- grf[i,j] + 
          scale_fill_manual(values=colors) +
          scale_color_manual(values=colors)  
      }
    }
  }
  else {
    grf <- ggpairs(data, columns = icol, aes(colour = colors))  + theme_bw(base_size = 10)
  }
  return(grf)
}

