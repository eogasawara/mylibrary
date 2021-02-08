# version 1.0
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myRelation.R")

cluster_evaluation <- function(cluster, attribute) {
  obj <- atr_transform(cluster)
  obj$attribute <- attribute
  
  class(obj) <- append("cluster_evaluation", class(obj))    
  return(obj)
}

prepare.cluster_evaluation <- function(obj) {
  
  clust_entropy <- function(class, cluster) {
    class <- as.factor(class)
    l <- split(class, cluster)
    entropy <- c(1:(length(l)+1))
    entropy[1] <- (Entropy(table(class), base=exp(1)))
    ctable <- table(class)
    
    for (i in 1:length(l)) {
      x <- factor(l[[i]], levels(class))
      entropy[i+1] <- Entropy(table(x), base=exp(1))
      ctable <- rbind(ctable, table(x))
    } 
    return (list(entropy=entropy, table=ctable)) 
  }
  
  return(obj)
}

action.classif_evaluation <- function(obj) {
  metrics <- data.frame(entropy=0)
  return(metrics)
}

