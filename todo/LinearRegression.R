
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myPreprocessing.R")

loadlibrary("MASS")
loadlibrary("plotly")
loadlibrary("reshape2")

plot_size(4, 3)

exp_table(t(sapply(Boston, class)))
exp_table(Boston)
?MASS::Boston

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

axis_x <- seq(min(Boston$lstat), max(Boston$lstat), by = 0.5)
axis_y <- seq(min(Boston$age), max(Boston$age), by = 0.5)

lm_surface <- expand.grid(lstat = axis_x, age = axis_y, KEEP.OUT.ATTRS = F)
lm_surface$medv <- predict.lm(lm.fit2, newdata = lm_surface)
lm_surface <- acast(lm_surface, age ~ lstat, value.var = "medv") #y ~ x

b3d_plot <- plot_ly(Boston, 
                     x = ~Boston$lstat, 
                     y = ~Boston$age, 
                     z = ~Boston$medv,
                     text = Boston$medv, 
                     type = "scatter3d",
                     mode = "markers"
)


b3d_plot <- add_trace(p = b3d_plot,
                       z = lm_surface,
                       x = axis_x,
                       y = axis_y,
                       type = "surface")

b3d_plot

set.seed(1)
exp_table(t(sapply(iris, class)))
exp_table(iris)

??datasets::iris

data <- iris
data$versicolor <- as.integer(data$Species=="versicolor")
data$Species <- c('other', 'versicolor')[data$versicolor+1]

sampler <- sample.random(data)
train <- sampler$sample
test <- sampler$residual
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


