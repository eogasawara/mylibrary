plot_correlation <- function(data, colors="") {
  if (colors == "")
    colors <- brewer.pal(n=8, name="RdYlBu")
  series <-cor(data)
  corrplot(series, type="upper", order="hclust", col=colors)
}


#'@title plot scatter
#'@description plot scatter
#'@return plot
#'@examples
#'@export
plot_norm_dist <- function(vect, label_x = "", label_y = "",  colors)  {
  data <- data.frame(value = vect)
  grf <- ggplot(data, aes(sample = value)) +
    stat_qq(color=colors) + xlab(label_x) + ylab(label_y) +
    theme_bw(base_size = 10) +
    stat_qq_line(color=colors)
  return (grf)
}


#'@title plot scatter
#'@description plot scatter
#'@return plot
#'@examples
#'@export
plot_pair <- function(data, cnames, title = NULL, clabel = NULL, colors) {
  grf <- PairPlot(data, cnames, title, group_var = clabel, palette=NULL) + theme_bw(base_size = 10)
  if (is.null(clabel))
    grf <- grf + geom_point(color=colors)
  else
    grf <- grf + scale_color_manual(values=colors)
  return (grf)
}

#'@title plot scatter
#'@description plot scatter
#'@return plot
#'@examples
#'@export
plot_pair_adv <- function(data, cnames, title = NULL, clabel= NULL, colors) {
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

#roc_curve
#'@import ROCR
#'@export
roc_curve <- function(data, prediction) {
  pred <- ROCR::prediction(prediction, data)
  rocr <- ROCR::performance(pred, "tpr", "fpr")
  return (rocr)
}




