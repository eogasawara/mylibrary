suppressPackageStartupMessages(source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myGraphic.R"))

col_set <- brewer.pal(11, 'Spectral')
colors <- col_set[c(3,5,7,9)]   

# This function is used only to set graphics size in this notebook. Ignore it for the moment.
plot.size(10, 5)

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

series <- mtcars %>% select(wt, mpg, disp)
head(series)

grf <- plot.scatter(series, label_x = "wt", colors=colors[1:2])
grf <- grf + theme(text = element_text(size=16))
plot(grf)

grf <- plot.series(series, label_x = "wt", colors=colors[1:2])
grf <- grf + theme(text = element_text(size=16))
plot(grf)

series <- mtcars %>% select(wt, mpg, disp)
grf <- plot.series2nd(series, label_x = "wt", colors=colors[1:2])
grf <- grf + theme(text = element_text(size=16))
plot(grf)

series <- mtcars %>% group_by(cyl) %>% summarize(hp = mean(hp))
head(series)

grf <- plot.bar(series, colors=col_set[1])
grf <- grf + theme(text = element_text(size=16))
plot(grf)

grf <- grf + coord_flip()
plot(grf)

data_sd <- flightdata %>% 
  select(variable=Quarters, value=Delays) %>% 
  group_by(variable) %>% 
  summarize(mean = mean(value), sd = sd(value))
head(data_sd)

grf <- plot.bar(data_sd, colors=colors[1], alpha=0.5)
grf <- grf + geom_errorbar(aes(x=variable, ymin=mean-sd, ymax=mean+sd), width=0.2, colour=colors[2], alpha=0.9, size=1.1) 
grf <- grf + theme(text = element_text(size=16))
plot(grf)

series <- mtcars %>% group_by(cyl) %>% summarize(hp = mean(hp), mpg = mean(mpg))
head(series)

grf <- plot.groupedbar(series, colors=colors[1:2])
grf <- grf + theme(text = element_text(size=16))
plot(grf)

grf <- plot.stackedbar(series, colors=colors[1:2])
grf <- grf + theme(axis.text.x = element_text(angle=90, hjust=1))
grf <- grf + theme(text = element_text(size=16))
plot(grf)

grf <- plot.pieplot(flightdata %>% group_by(Quarters) %>% summarize(Flights=mean(Flights)), colors=colors[1:4])
grf <- grf + theme(text = element_text(size=16))
plot(grf)

series <- mtcars %>% group_by(cyl) %>% summarize(hp = mean(hp))
head(series)

grf <- plot.radar(series, colors=colors[1])
grf <- grf + ylim(0, NA)
grf <- grf + theme(text = element_text(size=16))
plot(grf)

grf <- plot.lollipop(series, colors=col_set[1])
grf <- grf + theme(text = element_text(size=16))
plot(grf)

series <- mtcars %>% group_by(cyl) %>% summarize(hp = mean(hp), mpg = mean(mpg))
head(series)

grf <- plot.dotchar(series, colors=colors[1:2])
grf <- grf + theme(axis.text.x = element_text(angle=90, hjust=1))
plot(grf)

cor(mtcars)

plot.correlation(mtcars)

stdata <- data.frame(exponential = rexp(100000, rate = 1), 
                     uniform = runif(100000, min = 2.5, max = 3.5), 
                     normal = rnorm(100000, mean=5), 
                     poisson = rpois(100000, lambda = 2))
head(stdata)

grfe <- plot.hist(stdata %>% select(exponential), label_x = "exponential", color=colors[1])  
grfu <- plot.hist(stdata %>% select(uniform), label_x = "uniform", color=colors[1])  
grfn <- plot.hist(stdata %>% select(normal), label_x = "normal", color=colors[1])  
grfp <- plot.hist(stdata %>% select(poisson), label_x = "poisson", color=colors[1])  
loadlibrary("gridExtra") 
grid.arrange(grfe, grfu, grfn, grfp,ncol=2)

grfe <- plot.density(stdata %>% select(exponential), label_x = "exponential", color=colors[1])  
grfu <- plot.density(stdata %>% select(uniform), label_x = "uniform", color=colors[2])  
grfn <- plot.density(stdata %>% select(normal), label_x = "normal", color=colors[3])  
grid.arrange(grfe, grfu, grfn, ncol=3) 

grf <- plot.density(stdata %>% select(exponential, uniform, normal), colors=colors[1:3])
plot(grf)

grf <- plot.boxplot(stdata, colors=colors[1:4])
plot(grf)  

grf <- plot.norm_dist(rexp(1000, rate=1), colors=colors[1])
plot(grf)

pdf("examples/plot.pdf", width=4, height=3)
plot(grf)
dev.off()


