# DAL ToolBox
# version 1.0.77



library(daltoolbox)
library(MASS)
library(plotly)
library(reshape2)

head(Boston)

lm.fit = lm(medv ~ lstat, data = Boston)

summary(lm.fit)

predict(lm.fit, data.frame(lstat =(c(5, 10, 15))), interval = "confidence")
predict(lm.fit, data.frame(lstat =(c(5, 10, 15))), interval = "prediction")

axis_x <- seq(min(Boston$lstat), max(Boston$lstat), by = 0.5)
axis_y <- predict(lm.fit, data.frame(lstat=axis_x))

data_adj = data.frame(lstat=axis_x, medv=axis_y)

ggplot(Boston) + geom_point(aes(x = lstat, y = medv)) + geom_line(data=data_adj,aes(x=lstat,y=medv), color="Blue") + theme_bw(base_size = 10) 

lm.fit_p =lm(medv~lstat+I(lstat^2), data=Boston)
summary (lm.fit_p)

axis_x <- seq(min(Boston$lstat), max(Boston$lstat), by = 0.5)
axis_x2 <- axis_x^2
axis_y <- predict(lm.fit_p, data.frame(lstat=axis_x, `I(lstat^2)`=axis_x2))


data_adj = data.frame(lstat=axis_x, medv=axis_y)
ggplot(Boston) + geom_point(aes(x = lstat, y = medv)) + geom_line(data=data_adj,aes(x=lstat,y=medv), color="Blue") + theme_bw(base_size = 10) 

anova(lm.fit, lm.fit_p)

lm.fit2 =lm(medv~lstat+age, data=Boston)
summary (lm.fit2)

anova(lm.fit ,lm.fit2)

head(iris)

data <- iris
data$versicolor <- as.integer(data$Species=="versicolor")
data$Species <- c('other', 'versicolor')[data$versicolor+1]

tt <- daltoolbox::train_test(daltoolbox::sample_random(), data)
train <- tt$train
test <- tt$test
head(train)

t <- mean(train$versicolor)
print(t)

pred <- glm(versicolor ~ .-Species, data=train, family = binomial)

res <- predict(pred, train, type="response")
res <- as.integer(res >= t)
table(res, train$versicolor)

res <- predict(pred, test, type="response")
res <- res >= t
table(res, test$versicolor)

pred <- glm(versicolor ~ Petal.Length + Petal.Width, data=train, family = binomial)

res <- predict(pred, train, type="response")
res <- as.integer(res >= t)
table(res, train$versicolor)

res <- predict(pred, test, type="response")
res <- as.integer(res >= t)
table(res, test$versicolor)
