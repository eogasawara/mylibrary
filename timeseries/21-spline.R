df <- data.frame(x=1:20,
                 y=c(2, 4, 7, 9, 13, 15, 19, 16, 13, 10,
                     11, 14, 15, 15, 16, 15, 17, 19, 18, 20))

#view head of data frame
head(df)

#create scatterplot
plot(df$x, df$y, cex=1.5, pch=19)

#fit simple linear regression model
linear_fit <- lm(df$y ~ df$x)

#view model summary
summary(linear_fit)

#create scatterplot
plot(df$x, df$y, pch=19)
lines(df$x, linear_fit$fitted.values)

#add regression line to scatterplot
abline(linear_fit)

library(splines)

#fit spline regression model
spline_fit <- lm(df$y ~ bs(df$x))

#view summary of spline regression model
summary(spline_fit)

#create scatterplot
plot(df$x, df$y, pch=19)
lines(df$x, spline_fit$fitted.values)

