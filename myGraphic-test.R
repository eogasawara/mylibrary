# The easiest way to get ggplot2 is to install the whole tidyverse:
# install.packages("tidyverse")
# Alternatively, install just ggplot2:
# install.packages("ggplot2")
# Use suppressPackageStartupMessages(source(filename)) to avoid warning messages

source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myPackage.R")
loadlibrary("ggplot2")
loadlibrary("dplyr")
loadlibrary("reshape")
loadlibrary("RColorBrewer")
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myGraphic.R")


col_set <- brewer.pal(11, 'Spectral')
colors <- col_set[c(11,10,9,8)]

# This function is used only to set graphics size in this notebook. Ignore it for the moment.
plot.size(10, 5)

# setting the font size for all charts
font <- theme(text = element_text(size=16))

head(mtcars)

?mtcars

# example1: dataset to be plotted   
example1 <- mtcars %>% select(wt, mpg, disp)
head(example1)

# The function returns a preset graphic that can be enhanced. 
grf <- plot.scatter(example1, label_x = "wt", colors=colors[1:2])
# Increasing the font size of the graphics
grf <- grf + font
# Actual plot
plot(grf)

grf <- plot.series(example1, label_x = "wt", colors=colors[1:2]) + font
plot(grf)

grf <- plot.series2nd(example1, label_x = "wt", colors=colors[1:2]) + font
plot(grf)

# example2: dataset to be plotted  
example2 <- mtcars %>% group_by(cyl) %>% summarize(hp = mean(hp))
head(example2)

grf <- plot.bar(example2, colors=colors[1]) + font
plot(grf)

# Sometimes the bars can be plotted vertically. 
#Use function coord_flip() for that.
grf <- grf + coord_flip()
plot(grf)

grf <- plot.lollipop(example2, colors=colors[1]) + font
plot(grf)

grf <- plot.pieplot(example2, colors=colors[1:nrow(example2)]) + font
plot(grf)

grf <- plot.radar(example2, colors=colors[1]) + font
grf <- grf + ylim(0, NA)
plot(grf)

# example3: dataset to be plotted  
example3 <- mtcars %>% group_by(cyl) %>% summarize(mean = mean(hp), sd=sd(hp))
head(example3)

grf <- plot.bar(example3, colors=colors[1], alpha=1) + font
grf <- grf + geom_errorbar(aes(x=cyl, ymin=mean-sd, ymax=mean+sd), 
                           width=0.2, colour="darkgray", alpha=0.8, size=1.1) 
plot(grf)

grf <- plot.groupedbar(example3, colors=colors[1:2]) + font
plot(grf)

grf <- plot.stackedbar(example3, colors=colors[1:2]) + font
grf <- grf + theme(axis.text.x = element_text(angle=90, hjust=1))
plot(grf)

grf <- plot.dotchar(example3, colors=colors[1:2]) + font
grf <- grf + theme(axis.text.x = element_text(angle=90, hjust=1))
plot(grf)

# Correlation matrix
cor(mtcars)

plot.correlation(mtcars)

# example4: dataset to be plotted  
example4 <- data.frame(exponential = rexp(100000, rate = 1), 
                       uniform = runif(100000, min = 2.5, max = 3.5), 
                       normal = rnorm(100000, mean=5), 
                       poisson = rpois(100000, lambda = 2))
head(example4)

grf <- plot.hist(example4 %>% select(exponential), 
                 label_x = "exponential", color=colors[1]) + font
plot(grf)

grfe <- plot.hist(example4 %>% select(exponential), 
                  label_x = "exponential", color=colors[1]) + font
grfu <- plot.hist(example4 %>% select(uniform), 
                  label_x = "uniform", color=colors[1]) + font  
grfn <- plot.hist(example4 %>% select(normal), 
                  label_x = "normal", color=colors[1]) + font 
grfp <- plot.hist(example4 %>% select(poisson), 
                  label_x = "poisson", color=colors[1]) + font

loadlibrary("gridExtra") 
grid.arrange(grfe, grfu, grfn, grfp, ncol=2)

grfe <- plot.density(example4 %>% select(exponential), 
                     label_x = "exponential", color=colors[1]) + font  
grfu <- plot.density(example4 %>% select(uniform), 
                     label_x = "uniform", color=colors[2]) + font  
grfn <- plot.density(example4 %>% select(normal), 
                     label_x = "normal", color=colors[3]) + font  

loadlibrary("gridExtra") 
grid.arrange(grfe, grfu, grfn, ncol=3)

grf <- plot.density(example4 %>% select(exponential, uniform, normal), 
                    colors=colors[1:3]) + font
plot(grf)

grf <- plot.boxplot(example4, colors=colors[1:4]) + font
plot(grf)  

grf <- plot.norm_dist(rexp(1000, rate=1), colors=colors[1]) + font
plot(grf)

pdf("examples/plot.pdf", width=4, height=3)
plot(grf)
dev.off()


