# version 1.0
#source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myTransform.R")

categ_mapping <- function(attribute) {
  obj <- dal_transform()
  obj$attribute <- attribute
  class(obj) <- append("categ_mapping", class(obj))  
  return(obj)  
}

action.categ_mapping <- function(obj, data) {
  mdlattribute = formula(paste("~", paste(obj$attribute, "-1")))
  catmap <- model.matrix(mdlattribute, data=data)
  data <- cbind(data, catmap)
  return(data)
}

