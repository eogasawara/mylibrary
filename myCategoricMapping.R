# version 1.0
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myTransform.R")

categ_mapping <- function(attribute) {
  obj <- dal_transform()
  obj$attribute <- attribute
  class(obj) <- append("categ_mapping", class(obj))  
  return(obj)  
}

action.categ_mapping <- function(obj, obj_data) {
  data <- obj_data$data
  attribute <- obj$attribute  
  
  mdlattribute = formula(paste("~", paste(attribute, "-1")))
  catmap <- model.matrix(mdlattribute, data=data)
  obj_data$data <- cbind(data, catmap)
  return(obj_data)
}

