loadlibrary <- function(x, repos='http://cran.fiocruz.br') 
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x, repos=repos, dep=TRUE)
    require(x)
  }
}

loadlibrary("ggplot2")
loadlibrary("scales")
loadlibrary("ggpubr")

plot.scatter <- function(series, label_series = "", label_x = "", label_y = "", colors = NULL) {
  grf <- ggplot(data=series) + geom_point(aes(x = x, y = value, colour=variable), size=1)
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

plot.series <- function(series, label_series = "", label_x = "", label_y = "", colors = NULL) {
  grf <- ggplot(data=series) + geom_point(aes(x = x, y = value, colour=variable), size=1.5) + geom_line(aes(x = x, y = value, colour=variable, group=variable), size=1)
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

plot.bar <- function(series, label_series = "", label_x = "", label_y = "", colors = NULL, group=FALSE, alpha=1) {
  if (group) {
    grf <- ggplot(series, aes(x, value, fill=variable)) + geom_bar(stat = "identity",position = "dodge", alpha=alpha)
    if (!is.null(colors)) {
      grf <- grf + scale_fill_manual(label_series, values = colors)
    }
  }
  else {  
    grf <- ggplot(series, aes(variable, value))
    if (!is.null(colors)) {
      grf <- grf + geom_bar(stat = "identity",fill=colors, alpha=alpha)
    }
    else {  
      grf <- grf + geom_bar(stat = "identity", alpha=alpha)
    }    
  }
  grf <- grf + theme_bw(base_size = 10)
  grf <- grf + theme(panel.grid.minor = element_blank()) + theme(legend.position = "bottom")
  if("x" %in% colnames(series))
    grf <- grf + scale_x_discrete(limits = unique(series$x))
  grf <- grf + xlab(label_x)
  grf <- grf + ylab(label_y)
  return(grf)
}

plot.radar <- function(series, label_series = "", label_x = "", label_y = "", color = NULL)  {
  grf <- ggplot(data=series, aes(x=x, y=value, group=1))
  grf <- grf + geom_point(size=2, color=color)
  grf <- grf + geom_polygon(size = 1, alpha= 0.1, color=color)
  grf <- grf + theme_light()
  grf <- grf + coord_polar()
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

plot.hist <-  function(series, label_series = "", label_x = "", label_y = "", colors = NULL, bin = NULL, alpha=0.25) {
  if("variable" %in% colnames(series)) {
    grf <- ggplot(series, aes(x=value,fill=variable))
    if (is.null(bin)) 
      grf <- grf + geom_histogram(alpha = alpha)
    else 
      grf <- grf + geom_histogram(binwidth = bin, alpha = alpha)
  }  
  else {
    grf <- ggplot(series, aes(x=value))
    if (is.null(bin)) {
      if (!is.null(colors)) 
        grf <- grf + geom_histogram(fill=colors, alpha = alpha)
      else
        grf <- grf + geom_histogram(alpha = alpha)
    }
    else {
      if (!is.null(colors)) 
        grf <- grf + geom_histogram(binwidth = bin,fill=colors, alpha = alpha)
      else
        grf <- grf + geom_histogram(binwidth = bin, alpha = alpha)
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

plot.density <-  function(series, label_series = "", label_x = "", label_y = "", colors = NULL, bin = NULL, alpha=0.25) {
  if("variable" %in% colnames(series)) {
    grf <- ggplot(series, aes(x=value,fill=variable))
    if (is.null(bin)) 
      grf <- grf + geom_density(alpha = alpha)
    else 
      grf <- grf + geom_density(binwidth = bin, alpha = alpha)
  }  
  else {
    grf <- ggplot(series, aes(x=value))
    if (is.null(bin)) {
      if (!is.null(colors)) 
        grf <- grf + geom_density(fill=colors, alpha = alpha)
      else
        grf <- grf + geom_density(alpha = alpha)
    }
    else {
      if (!is.null(colors)) 
        grf <- grf + geom_histogram(binwidth = bin,fill=colors, alpha = alpha)
      else
        grf <- grf + geom_histogram(binwidth = bin, alpha = alpha)
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

plot.boxplot <- function(series, label_series = "", label_x = "", label_y = "", colors = NULL, barwith=0.25) {
  grf <- ggplot(aes(y = value, x = variable), data = series)
  if (!is.null(colors)) {
    grf <- grf + geom_boxplot(fill = colors, width=barwith)
  }
  else {
    grf <- grf + geom_boxplot(width=barwith)
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

plot_lollipop <- function(series, color, xlabel = "", ylabel = "", size_text=3, size_ball=8, alpha_ball=0.2, min_value=0, max_value_gap=1) {
  series$value <- round(series$value)
  grf <- ggplot(data=series, aes(x=variable, y=value, label=value)) +
    geom_segment(aes(x=variable, xend=variable, y=min_value, yend=(value-max_value_gap)), color=color, size=1) +
    geom_text(color="black", size=size_text) +
    geom_point(color=color, size=size_ball, alpha=alpha_ball) +
    theme_light() +
    coord_flip() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.border = element_blank(),
      axis.ticks.y = element_blank()
    ) +
    ylab(xlabel) + xlab(xlabel)   
  return(grf)
}

plot_dotchar <- function(series, color, colorline = "lightgray", xlabel = "", ylabel = "", legend.title = "", sorting="ascending") {
  grf <- ggdotchart(series, x = "x", y = "value",
                  color = "variable", size = 3,
                  add = "segment",
                  sorting = sorting,
                  add.params = list(color = colorline, size = 1.5),
                  position = position_dodge(0.3),
                  palette = color,
                  ggtheme = theme_pubclean(), xlab = xlabel, ylab=ylabel)
  grf <- ggpar(grf,legend.title = legend.title)
  return(grf)
}


plot_ballon <- function(series, color) {
  grf <- ggballoonplot(series, x = 'x', y = 'variable', size = 'radius', fill = 'value')
  grf <- grf + gradient_fill(col_4)
  grf <- grf + guides(size = FALSE)                         
  return(grf)
} 
