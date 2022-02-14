source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myBasic.R")
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myGraphic.R")

colors <- brewer.pal(11, 'Paired')[c(2,4,6)]
font <- theme(text = element_text(size=16))



wine = read.table(
  "http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", 
  header = TRUE, sep = ",")
colnames(wine) <- c('Type', 'Alcohol', 'Malic', 'Ash', 
                    'Alcalinity', 'Magnesium', 'Phenols', 
                    'Flavanoids', 'Nonflavanoids',
                    'Proanthocyanins', 'Color', 'Hue', 
                    'Dilution', 'Proline')

#exercise 1
#c
head(wine)

#d
table(wine$Type)
class(wine$Type)

#e
sapply(wine, class)

#f
res <- sapply(wine[,-1], summary)
head(res)



grf <- plot.hist(wine %>% dplyr::select(Alcohol), 
          label_x = "Alcohol", color=colors[1]) + font
plot(grf)

regress <- colnames(wine)[-1]
for (i in regress) {
  grf <- plot.hist(wine %>% dplyr::select(i), 
                   label_x = i, color=colors[1]) + font
  plot(grf)
}


grf <- plot.density(wine %>% dplyr::select(Alcohol), 
                 label_x = "Alcohol", color=colors[1]) + font
plot(grf)

regress <- colnames(wine)[-1]
for (i in regress) {
  grf <- plot.density(wine %>% dplyr::select(i), 
                   label_x = i, color=colors[1]) + font
  plot(grf)
}

grf <- plot.density.class(wine %>% dplyr::select(Type, Alcohol), 
            class_label="Type", label_x = "Alcohol", color=colors[c(1:3)]) 
plot(grf)

regress <- colnames(wine)[-1]
for (i in regress) {
  grf <- plot.density.class(wine %>% dplyr::select(Type, i), 
                            class_label="Type", label_x = i, color=colors[c(1:3)]) 
  plot(grf)
}



grf <- plot.boxplot(wine %>% dplyr::select(Alcohol), 
                    label_x = "Alcohol", color=colors[1]) + font
plot(grf)


regress <- colnames(wine)[-1]
for (i in regress) {
  grf <- plot.boxplot(wine %>% dplyr::select(i), 
                      label_x = i, color=colors[1]) + font
  plot(grf)
}


grf <- plot.boxplot.class(wine %>% dplyr::select(Type, Alcohol), 
           class_label="Type", label_x = "Alcohol", color=colors[c(1:3)]) 
plot(grf)


regress <- colnames(wine)[-1]
for (i in regress) {
  grf <- plot.boxplot.class(wine %>% dplyr::select(Type, i), 
                            class_label="Type", label_x = i, color=colors[c(1:3)]) 
  plot(grf)
}


plot.correlation(wine %>% 
                   dplyr::select(regress))


wine$Type <- as.factor(wine$Type)
grf <- plot.pair.adv(data=wine, cnames=regress, 
                        title="Wine", clabel='Type', colors=colors[1:3])
grf

wine$Type <- as.factor(wine$Type)
grf <- plot.pair.adv(data=wine, cnames=regress[c(1,6,7,12)], 
                     title="Wine", clabel='Type', colors=colors[1:3])
grf



grf <- ggparcoord(data = wine, columns = c(2:ncol(wine)), group=1) + 
    theme_bw(base_size = 10) + scale_color_manual(values=colors[1:3])
plot(grf)


grf <- ggparcoord(data = wine, columns = c(c(1,6,7,12)+1), group=1) + 
  theme_bw(base_size = 10) + scale_color_manual(values=colors[1:3])
plot(grf)

