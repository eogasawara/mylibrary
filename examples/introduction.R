install.packages("coefplot")
install.packages("ISwR")
install.packages("ggplot2")
install.packages("TSPred")

require(coefplot)
require(ISwR)
require(ggplot2)
require(TSPred)

4 * (6 + 5)

x = 2

x

is.numeric(x)

x = rnorm(1000)
plot(x)

y = c(3, 1, 3, 1, 3)
x = c(1, 2, 3, 4, 5)

x

x * 3 

x

x + y 

weight = c(60, 72, 57, 90, 95, 72)
weight

height = c(1.75, 1.80, 1.65, 1.90, 1.74, 1.91)
bmi = weight/height^2
bmi

sum(weight)
sum(weight)/length(weight)
mean(weight)


xbar = sum(weight)/length(weight)
weight - xbar
(weight - xbar)^2
sum((weight - xbar)^2)
sqrt(sum((weight - xbar)^2)/(length(weight) - 1))
sd(weight)

mean(x)
rnorm(15)

plot(height, weight)
plot(height, weight, pch=2)

args(plot.default)
?plot

hh = c(1.65, 1.70, 1.75, 1.80, 1.85, 1.90)
lines(hh, 22.5 * hh^2)

t.test(bmi, mu=22.5)

t.test(bmi, mu=15)

x = c(10, NA, 13)

mean(x)

mean(x, na.rm=TRUE)


x = c(red=1, blue=2, green=3)
x

names(x)
x["blue"]*x

names(x) = c("red", "green", "blue")
x["blue"]*x

x = 1:12
dim(x) = c(3,4)
x

x = matrix(1:12,nrow=3,byrow=T)
rownames(x) = LETTERS[1:3]
x
t(x)

pain = c(0,3,2,2,1)
fpain = factor(pain,levels=0:3)
levels(fpain) = c("none","mild","medium","severe")

fpain

as.numeric(fpain)

levels(fpain)

intake.pre = c(5260,5470,5640,6180,6390,
               + 6515,6805,7515,7515,8230,8770)
intake.post = c(3910,4220,3885,5160,5645,
                + 4680,5265,5975,6790,6900,7335)

mylist = list(before=intake.pre,after=intake.post)

mylist

mylist$before

d = data.frame(intake.pre,intake.post)
d
d$intake.pre

intake.post[intake.pre > 7000]

intake.post[intake.pre > 7000 | intake.pre < 6000]

d[d$intake.pre > 7000 | d$intake.pre < 6000,]


energy
exp.lean = energy$expend[energy$stature=="lean"]
exp.obese = energy$expend[energy$stature=="obese"]
l = split(energy$expend, energy$stature)
l

lapply(thuesen, mean, na.rm=T)

sapply(thuesen, mean, na.rm=T)

m <- as.matrix(thuesen)
apply(m, 1, min, na.rm=TRUE)
apply(m, 2, min, na.rm=TRUE)

tapply(energy$expend, energy$stature, median)

intake$post
sort(intake$post)
order(intake$post)
o = order(intake$post)
intake$post[o]
intake$pre[o]

intake
intake.sorted = intake[o,]

intake.sorted

double.num = function(x)
{
  return(x * 2)
}
double.num(5)

double.numpar = function(x)
{
  if (x %% 2 == 0) {
    return(x * 2)
  }
  else {
    return(x)
  }
}
double.numpar(5)
double.numpar(6)

x = intake.pre

for (i in 1:length(x)) { 
  x[i] = x[i] * 2 
}
x

for (a in (x)) { 
  a = a * 2 
  print (a)
}
x

wine = read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", 
                  header = TRUE, sep = ",")
head(wine)
save(wine, file="wine.RData")
rm(wine)
load("wine.RData")
write.table(wine, file="wine.csv", row.names=FALSE, quote = FALSE)

require(grDevices)
data(iris)
save(iris, file="iris.RData")
load(file="iris.RData")
head(iris)

