# version 1.0
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myRelation.R")

cluster_evaluation <- function(cluster, attribute) {
  obj <- atr_transform(cluster)
  obj$attribute <- attribute
  
  class(obj) <- append("cluster_evaluation", class(obj))    
  return(obj)
}

prepare.cluster_evaluation <- function(obj) {
  loadlibrary("dplyr")
  
  entropy <- function(obj) {
    base <- data.frame(x = obj$data, y = obj$attribute) 
    tbl <- base %>% group_by(x, y) %>% summarise(qtd=n()) 
    tbs <- base %>% group_by(x) %>% summarise(t=n()) 
    tbl <- merge(x=tbl, y=tbs, by.x="x", by.y="x")
    tbl$e <- -(tbl$qtd/tbl$t)*log(tbl$qtd/tbl$t,2)
    tbl <- tbl %>% group_by(x) %>% summarise(ce=sum(e), qtd=sum(qtd)) 
    tbl$ceg <- tbl$ce*tbl$qtd/length(obj$data)
    obj$entropy <- tbl
    return(obj)
  }
  
  obj <- entropy(obj)
  
  return(obj)
}

action.cluster_evaluation <- function(obj) {
  metrics <- data.frame(entropy=sum(obj$entropy$ceg))
  return(metrics)
}

