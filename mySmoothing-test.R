
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myPreprocessing.R")
loadlibrary("RColorBrewer")
loadlibrary("dplyr")
loadlibrary("gridExtra")
loadlibrary("reshape")

col.set <- brewer.pal(11, 'Spectral')
mycolors <- col.set[c(1,3,5,7,9)]

plot_size(4, 3)

bi <- smoothing.interval(iris$Sepal.Length, n=2)
bf <- smoothing.freq(iris$Sepal.Length, n=2)
bc <- smoothing.cluster(iris$Sepal.Length, n=2)

show_row(c('interval: ', sprintf("%.1f",bi$interval), 'entropy: ', sprintf("%.2f",smoothing_entropy(bi$bins_factor, iris$Species))))
show_row(c('freq: ', sprintf("%.1f",bf$interval), 'entropy: ', sprintf("%.2f",smoothing_entropy(bf$bins_factor, iris$Species))))
show_row(c('cluster: ', sprintf("%.1f",bc$interval), 'entropy: ', sprintf("%.2f",smoothing_entropy(bc$bins_factor, iris$Species))))

bsl <- smoothing.opt(iris$Sepal.Length, smoothing=smoothing.freq)
bsw <- smoothing.opt(iris$Sepal.Width, smoothing=smoothing.freq)
bpl <- smoothing.opt(iris$Petal.Length, smoothing=smoothing.freq)
bpw <- smoothing.opt(iris$Petal.Width, smoothing=smoothing.freq)


show_row(c('Sepal.Length: ', sprintf("%.1f",bsl$interval), 'entropy: ', sprintf("%.2f",smoothing_entropy(bsl$bins_factor, iris$Species))))
show_row(c('Sepal.Width: ', sprintf("%.1f",bsw$interval), 'entropy: ', sprintf("%.2f",smoothing_entropy(bsw$bins_factor, iris$Species))))
show_row(c('Petal.Length: ', sprintf("%.1f",bpl$interval), 'entropy: ', sprintf("%.2f",smoothing_entropy(bpl$bins_factor, iris$Species))))
show_row(c('Petal.Width: ', sprintf("%.1f",bpw$interval), 'entropy: ', sprintf("%.2f",smoothing_entropy(bpw$bins_factor, iris$Species))))

bsl <- smoothing.opt(iris$Sepal.Length, smoothing=smoothing.cluster)
bsw <- smoothing.opt(iris$Sepal.Width, smoothing=smoothing.cluster)
bpl <- smoothing.opt(iris$Petal.Length, smoothing=smoothing.cluster)
bpw <- smoothing.opt(iris$Petal.Width, smoothing=smoothing.cluster)


show_row(c('Sepal.Length: ', sprintf("%.1f",bsl$interval), 'entropy: ', sprintf("%.2f",smoothing_entropy(bsl$bins_factor, iris$Species))))
show_row(c('Sepal.Width: ', sprintf("%.1f",bsw$interval), 'entropy: ', sprintf("%.2f",smoothing_entropy(bsw$bins_factor, iris$Species))))
show_row(c('Petal.Length: ', sprintf("%.1f",bpl$interval), 'entropy: ', sprintf("%.2f",smoothing_entropy(bpl$bins_factor, iris$Species))))
show_row(c('Petal.Width: ', sprintf("%.1f",bpw$interval), 'entropy: ', sprintf("%.2f",smoothing_entropy(bpw$bins_factor, iris$Species))))

