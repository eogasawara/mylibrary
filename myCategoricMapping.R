# version 1.0
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myRelation.R")

categ_mapping <- function(data, attribute) {
  obj <- rel_transform(data)
  obj$attribute <- attribute
  class(obj) <- append("categ_mapping", class(obj))  
  return(obj)  
}

action.categ_mapping <- function(obj) {
  data <- obj$data
  attribute <- obj$attribute  
  
  mdlattribute = formula(paste("~", paste(attribute, "-1")))
  catmap <- model.matrix(mdlattribute, data=data)
  data <- cbind(data, catmap)
  return(data)
}

