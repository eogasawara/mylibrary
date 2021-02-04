source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myRelation.R")

# entropy

entropy <- function(data, attribute) {
  obj <- atr_transform(data)
  obj$attribute <- attribute
  class(obj) <- append("entropy", class(obj))    
  return(obj)
}


prepare.entropy <- function(obj) {
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

action.entropy <- function(obj) {
  return (sum(obj$entropy$ceg))
}


