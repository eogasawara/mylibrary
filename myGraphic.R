# version 0.9
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myData.R")

loadlibrary("ggplot2")
loadlibrary("scales")
#loadlibrary("ggpubr")
loadlibrary("reshape")

plot.scatter <- function(series, label_series = "", label_x = "", label_y = "", colors = NULL) {
  grf <- ggplot(data=series, aes(x = x, y = value, colour=variable, group=variable)) + geom_point(size=1)
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
  grf <- ggplot(data=series, aes(x = x, y = value, colour=variable, group=variable)) + geom_point(size=1.5) + geom_line(size=1)
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


plot.series2nd <- function(series, label_x = "x", label_y = "y", label_z = "z", colors = c("blue", "red")) {
  a            <- range(series$y)
  b            <- range(series$z)
  scale_factor <- diff(a)/diff(b)
  series$z <- ((series$z - b[1]) * scale_factor) + a[1]
  trans <- ~ ((. - a[1]) / scale_factor) + b[1]  
  
  grf <- ggplot(series) 
  grf <- grf + theme_bw(base_size = 10) 
  grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank()) 
  grf <- grf + theme(legend.position = "bottom") + theme(legend.key = element_blank()) 
  grf <- grf + geom_point(aes(x, y), col=colors[1], size=1.5) + geom_line(aes(x, y), col=colors[1]) 
  grf <- grf + geom_point(aes(x, z), col=colors[2], size=1.5) + geom_line(aes(x, z), col=colors[2])
  grf <- grf + xlab(label_x)
  grf <- grf + ylab(label_y)  
  grf <- grf + scale_y_continuous(sec.axis = sec_axis(trans=trans, name=label_z)) 
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

plot_lollipop <- function(series, color, xlabel = "", ylabel = "", size_text=3, size_ball=8, alpha_ball=0.2, min_value=0, max_value_gap=1, flip = TRUE) {
  series$value <- round(series$value)
  grf <- ggplot(data=series, aes(x=variable, y=value, label=value)) +
    geom_segment(aes(x=variable, xend=variable, y=min_value, yend=(value-max_value_gap)), color=color, size=1) +
    geom_text(color="black", size=size_text) +
    geom_point(color=color, size=size_ball, alpha=alpha_ball) +
    theme_light() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.border = element_blank(),
      axis.ticks.y = element_blank()
    ) +
    ylab(xlabel) + xlab(xlabel)   
  if (flip)
    grf <- grf + coord_flip()
	
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
  grf <- grf + gradient_fill(color)
  grf <- grf + guides(size = FALSE)                         
  return(grf)
} 


plot.hist <-  function(series, label_series = "", label_x = "", label_y = "", color = 'white', alpha=0.25) {
  tmp <- hist(series$value, plot = FALSE)
  grf <- ggplot(series, aes(x=value))
  grf <- grf + geom_histogram(breaks=tmp$breaks,fill=color, alpha = alpha, colour="black")
  grf <- grf + xlab(label_x)
  grf <- grf + ylab(label_y)
  grf <- grf + theme_bw(base_size = 10)
  grf <- grf + scale_fill_manual(name = label_series, values = color)
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
        grf <- grf + geom_density(binwidth = bin,fill=colors, alpha = alpha)
      else
        grf <- grf + geom_density(binwidth = bin, alpha = alpha)
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

plot_size <-function(width, height) {
  options(repr.plot.width=width, repr.plot.height=height)    
}

show_row <- function(x) {
  x <- data.frame(t(x))
  colnames(x) <- NULL
  return(head(x))
}

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

