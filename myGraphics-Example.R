#basedir <- "/Users/eogasawara/"
#basedir <- "C:/Users/eduar/"
#setwd(paste(basedir, "Dropbox/Git/mylibrary/", sep=""))

source("mygraphics.R")

loadlibrary("bibtex")
loadlibrary("network")
loadlibrary("RColorBrewer")
loadlibrary("igraph")
loadlibrary("ggplot2")
loadlibrary("gridExtra")
loadlibrary("plyr")
loadlibrary("readr")
loadlibrary("dplyr")
loadlibrary("reshape")
loadlibrary("tm")
loadlibrary("SnowballC")
loadlibrary("wordcloud")
loadlibrary("readxl")
loadlibrary("scales")
loadlibrary("GGally")
loadlibrary("ggthemes")
loadlibrary("repr")

dataset_series <- function() {
  series <- data.frame(x= 1:10, sin=sin(1:10), cos=cos(1:10))
  series <- melt(series[,c('x','sin','cos')],id.vars = 1)
  return(series)  
}
mydataseries <- dataset_series()

dataset_bar <- function() {
  series <- matrix(nrow=5, ncol=2)
  series[1,] = c("janeiro", 10)
  series[2,] = c("fevereiro", 9)
  series[3,] = c("março", 11)
  series[4,] = c("abril", 8)
  series[5,] = c("maio", 12)
  series <- data.frame(variable=as.factor(series[,1]), value=as.double(series[,2]))  
  return(series)
}
mydatabar <- dataset_bar()
save(mydatabar, file="mydatabar.RData") 


dataset_stackedbar <- function() {
  series <- read.table(text = "x Map Reduce 
                          janeiro 10 5
                          fevereiro 9 4
                          março 11 5
                          abril 8 4
                          maio 12 6",header = TRUE,sep = "")  
  series <- melt(series[,c('x','Map','Reduce')],id.vars = 1)
}
mydatastackedbar <- dataset_stackedbar()
save(mydatastackedbar, file="mydatastackedbar.RData") 

mynorm <- data.frame(value=rnorm(10000))

col.set <- brewer.pal(11, 'Spectral')
mycolors <- col.set[c(1,3,5,7,9)]

con <- url("https://github.com/eogasawara/mylibrary/raw/master/meses.RData")
load(con)

load(url("https://github.com/eogasawara/mylibrary/raw/master/mydatabar.RData"))

load(url("https://github.com/eogasawara/mylibrary/raw/master/mydatastackedbar.RData"))



grfs <- plot.series(mydataseries,colors=mycolors)
options(repr.plot.width=4, repr.plot.height=3)
plot(grfs)

grf <- plot.bar(mydatabar, colors=mycolors)
options(repr.plot.width=4, repr.plot.height=3)
plot(grf)

grfb <- plot.bar(mydatastackedbar, group=TRUE, colors=mycolors)
grfsb <- plot.stackedbar(mydatastackedbar, colors=mycolors)

options(repr.plot.width=7, repr.plot.height=3)
grid.arrange(grfb, grfsb, ncol=2)

grfa <- plot.bar(meses, group=TRUE, colors=mycolors)
grfa <- grfa + xlab("")
grfa <- grfa + guides(fill=guide_legend(title="velocidade"))
grfa <- grfa + ylab("anomalias")
grfa <- grfa + facet_wrap(~face, ncol = 3) 

options(repr.plot.width=7, repr.plot.height=3)
plot(grfa)

mymeses <- filter(meses, (face == "julho") & (variable == "Maior"))[, c("x", "value")]
names(mymeses) <- c("variable", "value")
mymeses$colors <- mycolors
mymeses <- prepare.pieplot(mymeses)
grfpie <- plot.pieplot(mymeses, label_x = "julho", colors=as.character(mymeses$colors))

options(repr.plot.width=4, repr.plot.height=3)
plot(grfpie)

series <- data.frame(variable=meses$x, value=meses$value)
grfgd <- plot.density(series, label_series = "distribuição", colors=mycolors)
plot(grfgd)

grfgh <- plot.hist(mynorm, label_series = "distribuição", colors=mycolors[1])
plot(grfgh)

grfb <- plot.boxplot(mydataseries, colors=mycolors[1:2])
plot(grfb)


pdf("myplot.pdf", width=7, height=3)
plot(grfa)
dev.off()


