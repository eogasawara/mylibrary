#install.packages("gplots")
#install.packages("ggplot2") 
#

library(ggplot2)
library(scales)

loadlibrary <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x, repos='http://cran.us.r-project.org', dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

loadlibrary("ggplot2")
loadlibrary("scales")

plot.series <- function(series, label_series = "", label_x = "", label_y = "", colors = NULL) {
  grf <- ggplot(data=series, aes(x = x, y = value, colour=variable))
  grf <- grf + geom_line() + geom_point(data=series, aes(x = x, y = value, colour=variable), size=0.5)
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

plot.bar <- function(series, label_series = "", label_x = "", label_y = "", colors = NULL, group=FALSE) {
  if (group) {
    grf <- ggplot(series, aes(x, value, fill=variable)) + geom_bar(stat = "identity",position = "dodge")
    if (!is.null(colors)) {
      grf <- grf + scale_fill_manual(label_series, values = colors)
    }
  }
  else {  
    grf <- ggplot(series, aes(variable, value))
    if (!is.null(colors)) {
      grf <- grf + geom_bar(stat = "identity",fill=colors)
    }
    else {  
      grf <- grf + geom_bar(stat = "identity")
    }    
  }
  grf <- grf + theme_bw(base_size = 10)
  grf <- grf + theme(panel.grid.minor = element_blank()) + theme(legend.position = "bottom")
  grf <- grf + scale_x_discrete(limits = unique(series$x))
  grf <- grf + xlab(label_x)
  grf <- grf + ylab(label_y)
  return(grf)
}

plot.stackedbar <- function(series, label_series = "", label_x = "", label_y = "", colors = NULL) {
  grf <- ggplot(series, aes(x=x, y=value, fill=variable)) + geom_bar(stat="identity", colour="white")
  if (!is.null(colors)) {
    grf <- grf + scale_fill_manual(label_series, values = colors)
  }
  grf <- grf + theme_bw(base_size = 10)
  grf <- grf + theme(panel.grid.minor = element_blank()) + theme(legend.position = "bottom")
  grf <- grf + scale_x_discrete(limits = unique(series$x))
  grf <- grf + xlab(label_x)
  grf <- grf + ylab(label_y)
  return(grf)
}

prepare.pieplot <- function(series) {
  series <- series[order(series$variable),]
  myorder <- length(series$variable):1
  mylevels <- series$variable[myorder]
  series$colors <- series$colors[myorder]
  series <- mutate(series, variable = factor(variable, levels = mylevels),
                   cumulative = cumsum(value),
                   midpoint = cumulative - value / 2,
                   label = paste(round(value / sum(value) * 100, 0), "%"))
  return(series)
}

plot.pieplot <- function(series, label_series = "", label_x = "", label_y = "", colors = NULL) {
  grf <- ggplot(series, aes(x="", y=value, fill=variable)) + geom_bar(width = 1, stat = "identity")
  grf <- grf + theme_minimal(base_size = 10)
  grf <- grf + coord_polar("y", start=0) 
  grf <- grf + geom_text(aes(x = 1.3, y = midpoint, label = label))
  grf <- grf + theme(panel.grid.minor = element_blank()) + theme(legend.position = "bottom")
  grf <- grf + xlab(label_x)
  grf <- grf + ylab(label_y)
  grf <- grf + theme(axis.text.x=element_blank(),axis.ticks = element_blank(), panel.grid = element_blank())
  if (!is.null(colors))
    grf <- grf + scale_fill_manual(label_series, values = colors)
  return(grf)
}

plot.hist <-  function(series, label_series = "", label_x = "", label_y = "", colors = NULL, bin = NULL) {
  if("variable" %in% colnames(series)) {
    grf <- ggplot(series, aes(x=value,fill=variable))
    if (is.null(bin)) 
      grf <- grf + geom_histogram()
    else 
      grf <- grf + geom_histogram(binwidth = bin)
  }  
  else {
    grf <- ggplot(series, aes(x=value))
    if (is.null(bin)) {
      if (!is.null(colors)) 
        grf <- grf + geom_histogram(fill=colors)
      else
        grf <- grf + geom_histogram()
    }
    else {
      if (!is.null(colors)) 
        grf <- grf + geom_histogram(binwidth = bin,fill=colors)
      else
        grf <- grf + geom_histogram(binwidth = bin)
    }
  }
  grf <- grf + xlab(label_x)
  grf <- grf + ylab(label_y)
  grf <- grf + theme_bw(base_size = 10)
  if (!is.null(colors)) 
    grf <- grf + scale_fill_manual(name = label_series, values = colors)
  grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank()) + theme(legend.position = "bottom")
  return(grf)
}

plot.density <-  function(series, label_series = "", label_x = "", label_y = "", colors = NULL, bin = NULL) {
  if("variable" %in% colnames(series)) {
    grf <- ggplot(series, aes(x=value,fill=variable))
    if (is.null(bin)) 
      grf <- grf + geom_density()
    else 
      grf <- grf + geom_density(binwidth = bin)
  }  
  else {
    grf <- ggplot(series, aes(x=value))
    if (is.null(bin)) {
      if (!is.null(colors)) 
        grf <- grf + geom_density(fill=colors)
      else
        grf <- grf + geom_density()
    }
    else {
      if (!is.null(colors)) 
        grf <- grf + geom_histogram(binwidth = bin,fill=colors)
      else
        grf <- grf + geom_histogram(binwidth = bin)
    }
  }
  grf <- grf + theme_bw(base_size = 10)
  grf <- grf + xlab(label_x)
  grf <- grf + ylab(label_y)
  if (!is.null(colors)) 
    grf <- grf + scale_fill_manual(name = label_series, values = colors)
  grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank()) + theme(legend.position = "bottom")
  return(grf)
}

plot.boxplot <- function(series, label_series = "", label_x = "", label_y = "", colors = NULL) {
  grf <- ggplot(aes(y = value, x = variable), data = series)
  if (!is.null(colors)) {
    grf <- grf + geom_boxplot(fill = colors)
  }
  else {
    grf <- grf + geom_boxplot()
  }
  grf <- grf + labs(color=label_series)
  if (!is.null(colors)) {
    grf <- grf + scale_fill_manual(label_series, values = colors)
  }
  grf <- grf + theme_bw(base_size = 10)
  grf <- grf + theme(panel.grid.minor = element_blank()) + theme(legend.position = "bottom")
  grf <- grf + xlab(label_x)
  grf <- grf + ylab(label_y)
  return(grf)
}



