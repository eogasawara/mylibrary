source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myPreprocessing.R")
loadlibrary("arules")
loadlibrary("arulesSequences")
loadlibrary("arulesViz")


data(AdultUCI)
dim(AdultUCI)

## remove attributes
AdultUCI$fnlwgt <- NULL
AdultUCI$"education-num" <- NULL

## map metric attributes
AdultUCI$age <- ordered(cut(AdultUCI$age, c(15,25,45,65,100)),
                              labels = c("Young", "Middle-aged", "Senior", "Old"))

AdultUCI$"hours-per-week" <- ordered(cut(AdultUCI$"hours-per-week",
                                             c(0,25,40,60,168)),
                                         labels = c("Part-time", "Full-time", "Over-time", "Workaholic"))

AdultUCI$"capital-gain" <- ordered(cut(AdultUCI$"capital-gain",
                                           c(-Inf,0,median(AdultUCI$"capital-gain"[AdultUCI$"capital-gain">0]),
                                             Inf)), labels = c("None", "Low", "High"))

AdultUCI$"capital-loss" <- ordered(cut(AdultUCI$"capital-loss",
                                           c(-Inf,0, median(AdultUCI$"capital-loss"[AdultUCI$"capital-loss">0]),
                                             Inf)), labels = c("None", "Low", "High"))

## create transactions
AdultTrans <- as(AdultUCI, "transactions")

#Neste caso as regras serão geradas com suporte de 0,5, confiança de 0,9, tamanho minimo = 2, ou seja, um atributo do lado esquerdo e um do lado direito, tamanho maximo=3, lado direito limitado ao valor do atributo capital-gain=None e lado esquerdo livre. Isso significa que estamos interessados em qualquer causa que provoque a consequência capital gain=None. 
rules <- apriori(AdultTrans, parameter=list(supp = 0.5, conf = 0.9, minlen=2, maxlen= 3, target = "rules"), appearance=list(rhs = c("capital-gain=None"),default="lhs"),control=NULL)
rules_a <- as(rules, "data.frame")
head(rules_a)

irules <- inspect(rules)

imrules <- interestMeasure(rules, transactions = AdultTrans)
head(imrules)

rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)

subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=TRUE)] <- FALSE
redundant <- colSums(subset.matrix, na.rm=TRUE) >= 1
which(redundant)

rules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned)

options(repr.plot.width=4, repr.plot.height=4)
plot(rules.pruned)

options(repr.plot.width=4, repr.plot.height=4)
plot(rules.pruned, method="paracoord", control=list(reorder=TRUE))


x <- read_baskets(con = system.file("misc", "zaki.txt", package = "arulesSequences"), info = c("sequenceID","eventID","SIZE"))
as(x, "data.frame")

s1 <- cspade(x, parameter = list(support = 0.4), control = list(verbose = TRUE))
as(s1, "data.frame")
