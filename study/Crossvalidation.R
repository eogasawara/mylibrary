
cross_val <- function(folds, clabel, method, tamanho = NULL, par_r = NULL, par_i =NULL) {
  val_temp = NULL
  cosmos_part = NULL
  
  method <- match.fun(method)
  
  for (i in 1:10)
  {
    cosmos_temp = NULL
    cosmos_acc = NULL
    cosmos_strat = NULL
    for (j in 1:10)
    {
      if (j==i)
      {
        next
      }
      cosmos_strat = folds[[j]]
      cosmos_acc = rbind(cosmos_temp, cosmos_strat)
      cosmos_temp = cosmos_acc
    }
    cosmos_part = folds[[i]]
    #x.method <- metodo(cosmos_acc, cosmos_part, tamanho, par_r, par_i)
    meth <- method(cosmos_acc, cosmos_part, clabel, tamanho, par_r, par_i)
    aa <- croc(meth, cosmos_part, "alvo")
    #aa <- croc(meth[,2], cosmos_part$alvo)
    #aa <- unlist(slot(aa, "y.values"))
    val_acum = c(aa, val_temp)
    val_temp = val_acum
    
  }
  med <- mean(val_acum)
  vrc <- var(val_acum)
  aa <- med - vrc
  return(aa)
  #return(meth)
}
