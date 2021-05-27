source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myGraphic.R")
loadlibrary("reshape")
loadlibrary("dplyr")
loadlibrary("RColorBrewer")
loadlibrary("gridExtra")

col_set <- brewer.pal(11, 'Spectral')
col_2 <- col_set[c(4,9)]
col_2b <- col_set[c(9,3)]    
col_3 <- col_set[c(4,9,11)]
col_4 <- col_set[c(3,5,7,9)]   

plot_size(12, 5)

create_dataset <- function() {
  data <- read.table(text = "Year Months Flights Delays
                     2016 Jan-Mar 11 6
                     2016 Apr-Jun 12 5
                     2016 Jul-Sep 13 3
                     2016 Oct-Dec 12 5
                     2017 Jan-Mar 10 4
                     2017 Apr-Jun 9 3
                     2017 Jul-Sep 11 4
                     2017 Oct-Dec 25 15
                     2018 Jan-Mar 14 3
                     2018 Apr-Jun 12 5
                     2018 Jul-Sep 13 3
                     2018 Oct-Dec 15 4",
                     header = TRUE,sep = "")  
  data$OnTime <- data$Flights - data$Delays 
  data$Perc <- round(100 * data$Delays / data$Flights)
  data$Months <- factor(data$Months, levels=c("Jan-Mar", "Apr-Jun", "Jul-Sep", "Oct-Dec"), ordered = TRUE)
  return(data)
}

flightdata <- create_dataset()
flightdata$x <- sprintf("%d-%s", flightdata$Year, flightdata$Months)
flightdata$x <- factor(flightdata$x, levels=flightdata$x, ordered=TRUE)
head(flightdata)

adjust_dataset <- function(data) {
  data <- melt(data[,c('Year', 'Months', 'Flights', 'Delays', 'OnTime', 'Perc')], id.vars = c(1,2))
  data$x <- sprintf("%d-%s", data$Year, data$Months)
  data$x <- factor(data$x,levels = data$x[1:12], ordered = TRUE)
  return(data)
}
data <- adjust_dataset(flightdata)
head(data)

levels(flightdata$x)

grf <- plot.scatter(data %>% filter(variable %in% c('Flights', 'Delays')), 
                     colors=col_2, label_x = "Quarters", label_y = "Amount") 
grf <- grf + theme(axis.text.x = element_text(angle=90, hjust=1))
grf <- grf + ggtitle("Air system") + theme(plot.title = element_text(hjust = 0.5))

plot(grf)

grf <- plot.series(data %>% filter(variable %in% c('Flights', 'Delays')),colors=col_2) 
grf <- grf + theme(axis.text.x = element_text(angle=45, hjust=1))

plot(grf)

series <- flightdata %>% select(x=x, y=Flights, z=Delays)
grf <- plot.series2nd(series, "Date", "Delays", "Delays",colors=col_2)
grf <- grf + theme(axis.text.x = element_text(angle=90, hjust=1))
plot(grf)

series <- mtcars %>% select(x=wt, y=mpg, z=disp)
grf <- plot.series2nd(series, "wt", "mpg", "disp",colors=col_2)
plot(grf)

library(corrplot)
library(RColorBrewer)
M <-cor(mtcars)
corrplot(M, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))

grf <- plot.bar(data[1:11,] %>% filter(variable=='Flights') %>% select(variable=x, value=value), colors=col_set)
grfA <- grf + theme(axis.text.x = element_text(angle=90, hjust=1))
grfB <- grf + coord_flip()

plot_size(24, 5)
grid.arrange(grfA, grfB, ncol=2)
plot_size(12, 5)

data_sd <- create_dataset() %>% 
  select(variable=Months, value=Delays) %>% 
  group_by(variable) %>% 
  summarize(sd = sd(value), value = mean(value))

data_sd$variable <- factor(data_sd$variable,levels = c('Jan-Mar','Apr-Jun','Jul-Sep','Oct-Dec'))

grf <- plot.bar(data_sd, colors=col_2[2], alpha=0.5)
grf <- grf + geom_errorbar(aes(x=variable, ymin=value-sd, ymax=value+sd), width=0.2, colour=col_2[2], alpha=0.9, size=1.1) 

plot(grf)


grfA <- plot.bar(data %>% filter(variable %in% c('OnTime', 'Delays')), colors=col_2b, group=TRUE)
grfA <- grfA + theme(axis.text.x = element_text(angle=90, hjust=1))
grfB <- plot.stackedbar(data %>% filter(variable %in% c('OnTime', 'Delays')), colors=col_2b) 
grfB <- grfB + theme(axis.text.x = element_text(angle=90, hjust=1))

plot_size(24, 5)
grid.arrange(grfA, grfB, ncol=2)
plot_size(12, 5)

grf <- plot.bar(data %>% filter(variable %in% c('OnTime', 'Delays')) %>% 
                select(x=Months, face=Year, variable=variable, value=value), group=TRUE, colors=col_2)
grf <- grf + theme(axis.text.x = element_text(angle=90, hjust=1))
grf <- grf + facet_wrap(~face, ncol = 3) 
grf <- grf + xlab("data")
grf <- grf + guides(fill=guide_legend(title="faced graphics:"))
grf <- grf + ylab("amount")

plot_size(24, 5)
plot(grf)
plot_size(12, 5)

mypiedata <- data %>% filter(Year == 2016 & variable =="Flights") %>% select(variable=Months, value=value)
mypiedata$variable <- factor(mypiedata$variable,levels = c('Oct-Dec', 'Jul-Sep', 'Apr-Jun', 'Jan-Mar'))
mypiedata$colors <- col_4
mypiedata <- prepare.pieplot(mypiedata)

grf <- plot.pieplot(mypiedata, colors=as.character(mypiedata$colors))

plot(grf)

radar_data <- data %>% filter(Year==2016 & variable=='Delays') %>% select(x = Months, value=value)
radar_data$x <- factor(radar_data$x,levels = c('Jan-Mar','Apr-Jun','Jul-Sep','Oct-Dec'))

grf <- plot.radar(series=radar_data, color=col_2[1])
grf <- grf + ylim(0, round(max(radar_data$value)))

plot(grf)

grf <- plot_lollipop(data %>% filter(variable =="Perc") %>% select(variable=x,value=value), col_2[1], max_value_gap=2.25)

plot(grf)

dotdata <- data %>% filter(variable %in% c("Delays", "OnTime") & Year==2016) %>% select(variable = variable, x = Months, value = value)
head(dotdata)

grf <- plot_dotchar(dotdata, color=col_2)
grf <- ggpar(grf,legend.title = 'Flights')

plot(grf)

data_ballon <- data %>%  filter(variable == "Flights") %>% select(variable=Year, x=Months, value=value)
data_ballon$variable <- factor(data_ballon$variable,levels = sort(unique(data_ballon$variable)))
data_ballon$x <- factor(data_ballon$x,levels = c('Jan-Mar','Apr-Jun','Jul-Sep','Oct-Dec'))
data_ballon$radius <- data_ballon$value

grf <- plot_ballon(data_ballon, color = col_set[c(6,3)])

plot(grf)

stdata <- data.frame(exponential = rexp(100000, rate = 1), 
                     uniform = runif(100000, min = 5.5, max = 6.5), 
                     normal = rnorm(100000, mean=5), 
                     poisson = rpois(100000, lambda = 2))
head(stdata)
stdata <- melt(stdata[,c('exponential', 'uniform', 'normal', 'poisson')])
head(stdata)

grfe <- plot.hist(stdata %>% filter(variable=='exponential'), label_x = "exponential", color=col_4[1])
grfu <- plot.hist(stdata %>% filter(variable=='uniform'), label_x = "uniform", color=col_4[2])
grfn <- plot.hist(stdata %>% filter(variable=='normal'), label_x = "normal", color=col_4[3])
grfp <- plot.hist(stdata %>% filter(variable=='poisson'), label_x = "poisson", color=col_4[4])

plot_size(24, 10)
grid.arrange(grfe, grfu, grfn, grfp,ncol=2)
plot_size(12, 5)

grf <- plot.density(stdata, label_series = "", colors=col_4)

plot(grf)

grf <- plot.boxplot(stdata, colors=col_4)
plot(grf)

pdf("examples/plot.pdf", width=4, height=3)
plot(grf)
dev.off()


