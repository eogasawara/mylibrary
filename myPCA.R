# version 1.0
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myRelation.R")
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myFitting.R")

# Data Transformation

# PCA

dt_pca <- function(data, attribute=NULL) {
  obj <- rel_transform(data)
  obj$attribute <- attribute
  class(obj) <- append("dt_pca", class(obj))    
  return(obj)
}  

prepare.dt_pca <- function(obj) {
  data <- data.frame(obj$data)
  attribute <- obj$attribute
  if (!is.null(attribute)) {
    data[,attribute] <- NULL
  }
  nums <- unlist(lapply(data, is.numeric))
  remove <- NULL
  for(j in names(nums[nums])) {
    if(min(data[,j])==max(data[,j]))
      remove <- cbind(remove, j)
  }
  nums[remove] <- FALSE
  data = as.matrix(data[ , nums])
  
  pca_res <- prcomp(data, center=TRUE, scale.=TRUE)
  cumvar <-  cumsum(pca_res$sdev^2/sum(pca_res$sdev^2))
  curv <-  curvature_min(cumvar)
  curv <- prepare(curv)
  res <- action(curv)
  
  pca.transf <- as.matrix(pca_res$rotation[, 1:res$x])
  
  obj$transf <- list(pca.transf=pca.transf, nums=nums)
  
  return(obj)
}

action.dt_pca <- function(obj) {
  data <- data.frame(obj$data)
  attribute <- obj$attribute
  pca.transf <- obj$transf$pca.transf
  nums <- obj$transf$nums
  
  if (!is.null(attribute)) {
    predictand <- data[,attribute]
    data[,attribute] <- NULL
  }

  data = as.matrix(data[ , nums])
  data = data %*% pca.transf
  data = data.frame(data)
  if (!is.null(attribute)){
    data[,attribute] <- predictand
  }
  return(data) 
}  
