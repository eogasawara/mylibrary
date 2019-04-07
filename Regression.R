source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myPreprocessing.R")
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myExploratory.R")

loadlibrary("MASS")
loadlibrary("plotly")
loadlibrary("reshape2")

exp_table(t(sapply(Boston, class)))
exp_table(Boston)
?MASS::Boston

lm.fit =lm(medv ~ lstat, data=Boston)
summary(lm.fit)

predict(lm.fit, data.frame(lstat =(c(5, 10, 15))), interval = "confidence")
predict(lm.fit, data.frame(lstat =(c(5, 10, 15))), interval = "prediction")
#https://statisticsbyjim.com/hypothesis-testing/confidence-prediction-tolerance-intervals/

axis_x <- seq(min(Boston$lstat), max(Boston$lstat), by = 0.5)
axis_y <- predict(lm.fit, data.frame(lstat=axis_x))

data_adj = data.frame(lstat=axis_x, medv=axis_y)

ggplot(Boston) + geom_point(aes(x = lstat, y = medv)) + geom_line(data=data_adj,aes(x=lstat,y=medv)) + theme_bw(base_size = 10) 

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


lm.fiti2 =lm(medv~lstat+I(lstat^2), data=Boston)
summary (lm.fiti2)

anova(lm.fit, lm.fiti2)


#Logist Regression

set.seed(1)
data <- iris
data$versicolor <- as.integer(data$Species=="versicolor")
cSpecies <- c('other', 'versicolor')
data$Species <- cSpecies[data$versicolor+1]

sampler <- sample.random(data)
train <- sampler$sample
test <- sampler$residual

t <- mean(train$versicolor)
print(t)

pred <- glm(versicolor ~ .-Species, data=train, family = binomial)
res <- predict(pred, test, type="response")
res <- res >= t
table(res, test$versicolor)

pred <- glm(versicolor ~ Sepal.Width, data=train, family = binomial)
res <- predict(pred, test, type="response")
res <- res >= t
table(res, test$versicolor)

pred <- glm(versicolor ~ Petal.Length, data=train, family = binomial)
res <- predict(pred, test, type="response")
res <- res >= t
table(res, test$versicolor)

pred <- glm(versicolor ~  Petal.Width, data=train, family = binomial)
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

iris_plot <- plot_ly(train, 
                     x = ~train$Petal.Length, 
                     y = ~train$Petal.Width, 
                     z = ~train$versicolor,
                     text = train$Species, 
                     type = "scatter3d",
                     color = ~train$Species,
                     colors = c("red","blue"),
                     mode = "markers")

iris_plot

#pred <- glm(versicolor ~ 0 + Petal.Length + Petal.Width, data=train, family = binomial)
pred <- lm(versicolor ~ 0 + Petal.Length + Petal.Width, data = train)


t <- mean(train$versicolor)
#t <- 0.5
print(t)

res <- predict(pred, train, type="response")
res <- as.integer(res >= t)
table(res, train$versicolor)

res <- predict(pred, test, type="response")
res <- as.integer(res >= t)
table(res, test$versicolor)



axis_x <- seq(min(train$Petal.Length), max(train$Petal.Length), by = 0.1)
axis_y <- seq(min(train$Petal.Width), max(train$Petal.Width), by = 0.1)

#Sample points
petal_lm_surface <- expand.grid(Petal.Length = axis_x, Petal.Width = axis_y, KEEP.OUT.ATTRS = F)
petal_lm_surface$versicolor <- predict.lm(pred, newdata = petal_lm_surface)
petal_lm_surface$versicolor <- as.integer(petal_lm_surface$versicolor >= t)
petal_lm_surface <- acast(petal_lm_surface, Petal.Width ~ Petal.Length, value.var = "versicolor") #y ~ x

iris_plot <- plot_ly(train, 
                     x = ~train$Petal.Length, 
                     y = ~train$Petal.Width, 
                     z = ~train$versicolor,
                     text = train$Species, 
                     type = "scatter3d",
                     color = ~train$Species,
                     colors = c("red","blue"),
                     mode = "markers")

iris_plot <- add_trace(p = iris_plot,
                       z = petal_lm_surface,
                       x = axis_x,
                       y = axis_y,
                       type = "surface")

iris_plot


