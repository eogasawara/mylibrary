
#link do IBX-50
#https://sistemaswebb3-listados.b3.com.br/indexPage/day/IBXL?language=pt-br


# Carregar bibliotecas
library(quantmod)

ibx50 <- read_excel("ibx50.xlsx")
dataset <- list()
for (i in 1:nrow(ibx50)) {
  ticker <- sprintf("%s.SA", ibx50$Code[i])
  data_inicio <- as.Date('2000-01-01')
  data_fim <- Sys.Date()
  data <- getSymbols(ticker, src = "yahoo", from = data_inicio, to = data_fim)
  prices <- get(ticker)
  dataset[[i]] <- data.frame(date = index(prices),
  open   = as.numeric(Op(prices)),
  high   = as.numeric(Hi(prices)),
  low    = as.numeric(Lo(prices)),
  close  = as.numeric(Cl(prices)),
  volume = as.numeric(Vo(prices)))  
  names(dataset) <- ibx50$Code[i]
}

