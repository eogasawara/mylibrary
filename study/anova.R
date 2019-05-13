setwd('/Users/eogasawara/Dropbox/Eduardo/R/anova/')
require(plotrix)

param <- read.table('weekly.csv', header = TRUE, sep = ",")
param$x <- factor(param$trainrangeinyears)
lm.fit <- lm(param$diff~param$x)
anova(lm.fit)
x <- confint(lm.fit)
x <- cbind(x, (x[,2]+x[,1])/2)
plotCI(1:12,x[,3], ui=x[,2], li=x[,1])

#visualmente observa-se que as tres primeiras colunas são as responsáveis pelas diferenças

subparam <- param[param$trainrangeinyears >= 4,]
lm.fit <- lm(subparam$diff~subparam$x)
anova(lm.fit)
x <- confint(lm.fit)
x <- cbind(x, (x[,2]+x[,1])/2)
plotCI(4:12,x[,3], ui=x[,2], li=x[,1])

#neste novo plot temos praticamente sobreposição de todos com todos.




