#install.packages("reshape")
library(reshape)


dataset_series <- function() {
  series <- data.frame(x= 1:10, sin=sin(1:10), cos=cos(1:10))
  series <- melt(series[,c('x','sin','cos')],id.vars = 1)
  return(series)  
}

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

dataset_stackedbar <- function() {
  series <- read.table(text = "x Map Reduce 
                          janeiro 10 5
                          fevereiro 9 4
                          março 11 5
                          abril 8 4
                          maio 12 6",header = TRUE,sep = "")  
  series <- melt(series[,c('x','Map','Reduce')],id.vars = 1)
}

mycolors=c("darkblue", "darkred", "darkgreen", "orange", "purple")
series <- dataset_series()
grf <- plot.series(series,colors=mycolors)
grf

series <- dataset_series()
grf <- plot.boxplot(series, colors=mycolors[1])
grf

series <- dataset_bar()
grf <- plot.bar(series, colors=mycolors)
grf

series <- dataset_stackedbar()
grf <- plot.bar(series, group=TRUE, colors=mycolors)
grf

series <- dataset_stackedbar()
grf <- plot.stackedbar(series, colors=mycolors)
grf

con <- url("https://github.com/eogasawara/mylibrary/raw/master/meses.RData")
load(con)
grf <- plot.bar(meses, group=TRUE, colors=mycolors)
grf <- grf + xlab("")
grf <- grf + guides(fill=guide_legend(title="velocidade"))
grf <- grf + ylab("anomalias")
grf <- grf + facet_wrap(~face, ncol = 3) 
grf 


ggsave( "myplot.pdf", width = 5.5, height = 4)

