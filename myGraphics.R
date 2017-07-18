#install.packages("gplots")
#install.packages("ggplot2")

library(ggplot2)
library(scales)

plot.series <- function(series, label_series=" ", label_x="x", label_y="y", colors=NULL) {
  grf <- ggplot(data=series, aes(x = x, y = y, colour=class))
  grf <- grf + geom_line() + geom_point(data=series, aes(x = x, y = y, colour=class), size=0.5)
  if (!is.null(colors)) {
    grf <- grf + scale_color_manual(values=colors)
  }
  grf <- grf + labs(color=label_series)
  grf <- grf + xlab(label_x)
  grf <- grf + ylab(label_y) 
  grf <- grf + theme_bw(base_size = 10)
  grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank()) 
  grf <- grf + theme(legend.position = "bottom") + theme(legend.key = element_blank()) 
  return(grf)
}

plot.boxplot <- function(series, labx = "x", laby = "y", colors = NULL) {
  grf <- ggplot(aes(y = y, x = class), data = series)
  if (!is.null(colors)) {
    grf <- grf + geom_boxplot(color = colors)
  }
  else {
    grf <- grf + geom_boxplot()
  }
  grf <- grf + theme_bw(base_size = 10)
  grf <- grf + theme(panel.grid.minor = element_blank()) + theme(legend.position = "bottom")
  grf <- grf + xlab(labx)
  grf <- grf + ylab(laby)
  return(grf)
}

plot.bar <- function(series, labx="class", laby="y", group=NULL, colors=NULL) {
  if (!is.null(group)) {
    grf <- ggplot(series, aes(class, y, fill=group)) + geom_bar(stat = "identity",position = "dodge")
    if (!is.null(colors)) {
      grf <- grf + scale_fill_manual("legend", values = colors)
    }
  }
  else {  
    grf <- ggplot(series, aes(class, y))
    if (!is.null(colors)) {
      grf <- grf + geom_bar(stat = "identity",fill=colors)
    }
    else {  
      grf <- grf + geom_bar(stat = "identity")
    }    
  }
  grf <- grf + theme_bw(base_size = 10)
  grf <- grf + theme(panel.grid.minor = element_blank()) + theme(legend.position = "bottom")
  grf <- grf + scale_x_discrete(limits = unique(series$class))
  return(grf)
}

plot.stackedbar <- function(series, labx="class", laby="y", colors=NULL) {
  grf <- ggplot(series, aes(x=class, y=y, fill=group)) + geom_bar(stat="identity", colour="white")
  if (!is.null(colors)) {
    grf <- grf + scale_fill_manual("legend", values = colors)
  }
  grf <- grf + theme_bw(base_size = 10)
  grf <- grf + theme(panel.grid.minor = element_blank()) + theme(legend.position = "bottom")
  grf <- grf + scale_x_discrete(limits = unique(series$class))
  return(grf)
}

