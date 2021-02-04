source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myGraphic.R")

#loadlibrary("caret")
#loadlibrary("MASS")
#loadlibrary("dplyr")


dt.categ_mapping <- function(data, attribute){
  mdlattribute = formula(paste("~", paste(attribute, "-1")))
  x <- model.matrix(mdlattribute, data=data)
  data <- cbind(data, x)
  return(data)
}

