source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myPreprocessing.R")
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myFeature.R")
data(iris)

# FEATURE SELECTION

myfeature <- fs.lasso(iris, "Species")
print(myfeature$features)

myfeature <- fs.fss(iris, "Species")
print(myfeature$features)

myfeature <- fs.cfs(iris, "Species")
print(myfeature$features)

options(repr.plot.width=3, repr.plot.height=3)
myfeature <- fs.ig(iris, "Species")
print(myfeature$features)

options(repr.plot.width=3, repr.plot.height=3)
myfeature <- fs.relief(iris, "Species")
print(myfeature$features)

