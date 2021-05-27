#source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myGraphic2.R")

#library(gridExtra)

col_set <- brewer.pal(11, 'Spectral')
col_2 <- col_set[c(4,9)]
col_3 <- col_set[c(4,9,11)]
col_4 <- col_set[c(3,5,7,9)]   

create_dataset <- function() {
  data <- read.table(text = "Year Quarters Flights Delays
                     2016 Q1 11 6
                     2016 Q2 12 5
                     2016 Q3 13 3
                     2016 Q4 12 5
                     2017 Q1 10 4
                     2017 Q2 9 3
                     2017 Q3 11 4
                     2017 Q4 25 15
                     2018 Q1 14 3
                     2018 Q2 12 5
                     2018 Q3 13 3
                     2018 Q4 15 4",
                     header = TRUE,sep = "")  
  data$OnTime <- data$Flights - data$Delays 
  data$Perc <- round(100 * data$Delays / data$Flights)
  data$Quarters <- factor(data$Quarters, levels=c("Q1", "Q2", "Q3", "Q4"), ordered = TRUE)
  return(data)
}
flightdata <- create_dataset()
head(flightdata)

head(mtcars)

if (TRUE) {
  cor(mtcars)
  plot.correlation(mtcars)
}

if (TRUE) {
  series <- mtcars %>% select(wt, mpg, disp)
  grf <- plot.scatter(series, label_x = "wt", colors=col_2)
  plot(grf)
}

if (TRUE) {
  series <- mtcars %>% select(wt, mpg, disp)
  grf <- plot.series(series, label_x = "wt", colors=col_2)
  plot(grf)
}

if (TRUE) {
  series <- mtcars %>% select(wt, mpg, disp)
  grf <- plot.series2nd(series, label_x = "wt", colors=col_2)
  plot(grf)
}

if (TRUE) {
  series <- mtcars %>% group_by(cyl) %>% summarize(hp = mean(hp))
  grf <- plot.bar(series, colors=col_set[1])
  grf <- grf + coord_flip()
  plot(grf)
}

if (TRUE) {
  series <- mtcars %>% group_by(cyl) %>% summarize(hp = mean(hp), mpg = mean(mpg))
  grf <- plot.bar(series, colors=col_2)
  plot(grf)
}

if (TRUE) {
  series <- mtcars %>% group_by(cyl) %>% summarize(hp = mean(hp), mpg = mean(mpg))
  grf <- plot.dotchar(series, colors=col_2)
  grf <- grf + theme(axis.text.x = element_text(angle=90, hjust=1))
  plot(grf)
}

if (TRUE) {
  series <- mtcars %>% group_by(cyl) %>% summarize(hp = mean(hp), mpg = mean(mpg))
  grf <- plot.stackedbar(series, colors=col_2)
  grf <- grf + theme(axis.text.x = element_text(angle=90, hjust=1))
  plot(grf)
}


if (TRUE) {
  series <- mtcars %>% group_by(cyl) %>% summarize(hp = mean(hp))
  grf <- plot.radar(series, colors=col_set[1])
  plot(grf)
}

if (TRUE) {
  series <- mtcars %>% group_by(cyl) %>% summarize(hp = mean(hp))
  grf <- plot_lollipop(series, colors=col_set[1])
  plot(grf)
}

if (TRUE) {
  series <- mtcars %>% group_by(cyl) %>% summarize(hp = mean(hp))
  grf <- plot.pieplot(series, colors=col_set[1:nrow(series)])
  plot(grf)
}


if (TRUE) {
  grf <- plot.pieplot(flightdata %>% group_by(Quarters) %>% summarize(Flights=mean(Flights), colors=col_4)
                      plot(grf)
}

if (TRUE) {
  loadlibrary("gridExtra")  
  stdata <- data.frame(exponential = rexp(100000, rate = 1), 
                       uniform = runif(100000, min = 2, max = 3), 
                       normal = rnorm(100000, mean=5), 
                       poisson = rpois(100000, lambda = 2))
  
  grfe <- plot.hist(stdata %>% select(exponential), label_x = "exponential", color=col_4[1])  
  grfu <- plot.hist(stdata %>% select(uniform), label_x = "uniform", color=col_4[1])  
  grfn <- plot.hist(stdata %>% select(normal), label_x = "normal", color=col_4[1])  
  grfp <- plot.hist(stdata %>% select(poisson), label_x = "poisson", color=col_4[1])  
  grid.arrange(grfe, grfu, grfn, grfp,ncol=2)
  
  grfe <- plot.density(stdata %>% select(exponential), label_x = "exponential", color=col_3[1])  
  grfu <- plot.density(stdata %>% select(uniform), label_x = "uniform", color=col_3[2])  
  grfn <- plot.density(stdata %>% select(normal), label_x = "normal", color=col_3[3])  
  grid.arrange(grfe, grfu, grfn, ncol=3)  
  
  grf <- plot.density(stdata %>% select(exponential, uniform, normal), colors=col_3)
  plot(grf)
  
  grf <- plot.boxplot(stdata, colors=col_4)
  plot(grf)  
  
  
  grf <- plot.norm_dist(rexp(1000, rate=1), colors=col_3[1])
  plot(grf)
}