---
title: "Change detection ewma"
date: "2026-03-23"
output:
  html_document:
    toc: true
    toc_depth: 2
---

Overview

This R Markdown document preserves the original example script and adds enough context to help you study, adapt, and rerun the workflow safely.

12-change-detection-ewma.R EWMA-based change detection using a simple Harbinger-style interface.

How To Read

- Comece pelo resumo em `Overview` para entender o objetivo do exemplo antes de olhar o codigo.
- Revise caminhos de arquivos, pacotes e dependencias externas antes de adaptar o script ao seu ambiente.
- Use este documento como material de estudo: primeiro entenda o fluxo, depois ajuste entradas, credenciais e saidas para o seu caso.

Execution Notes

- O chunk principal foi mantido com `eval=FALSE` para que a documentacao possa ser convertida com seguranca, mesmo quando houver dependencias externas, APIs, Python auxiliar ou datasets locais indisponiveis.
- Para executar o exemplo de verdade, rode o codigo em uma sessao interativa ou remova `eval=FALSE` depois de conferir caminhos, pacotes e arquivos auxiliares.
- Mantenha os arquivos desta pasta juntos, porque varios exemplos dependem de recursos vizinhos como `.py`, `.csv`, `.xlsx`, `.RData`, imagens ou `README`.

Original Script

The chunk below reproduces the original script with minimal structural changes so the example remains faithful to the source material.


``` r
## 12-change-detection-ewma.R
## EWMA-based change detection using a simple Harbinger-style interface.

source("https://raw.githubusercontent.com/eogasawara/TSED/refs/heads/main/code/header.R")

library(readxl)
library(daltoolbox)
library(dplyr)
library(stats)

hcp_ewma <- function(lambda = 0.2) {
  obj <- harbinger()
  obj$lambda <- lambda
  class(obj) <- append("hcp_ewma", class(obj))
  return(obj)
}

detect.hcp_ewma <- function(obj, serie, ...) {
  library(strucchange)
  if (is.null(serie)) stop("No data was provided for computation", call. = FALSE)

  non_na <- which(!is.na(serie))
  data <- serie[non_na]

  lambda <- obj$lambda  # Smoothing parameter
  n <- length(data)
  ewma <- numeric(n)
  ewma[1] <- data[1]
  for (i in 2:n) {
    ewma[i] <- lambda * data[i] + (1 - lambda) * ewma[i - 1]
  }
  x <- 1:n

  breaks <- breakpoints(y ~ x)

  inon_na <- rep(FALSE, length(non_na))
  nb <- length(breaks$breakpoints)
  if (nb > 0) inon_na[breaks$breakpoints] <- TRUE

  i <- rep(NA, length(serie))
  i[non_na] <- inon_na

  detection <- data.frame(idx = 1:length(serie), event = i, type = "")
  detection$type[i] <- "changepoint"
  return(detection)
}

set.seed(1)
n <- 100  # Number of time points
data <- c(sin((1:n)/pi), 2*sin((1:n)/pi), 10 + sin((1:n)/pi), 10-10/n*(1:n)+sin((1:n)/pi)/2, sin((1:n)/pi)/2)

model <- fit(hcp_ewma(), data)
detection <- detect(model, data)

grf <- har_plot(model, data, detection)
grf <- grf + ylab("value") + font

mypng(file = "figures/chap4_ewma.png", width = 1600, height = 720)
gridExtra::grid.arrange(grf, layout_matrix = matrix(c(1), byrow = TRUE, ncol = 2))
dev.off()
```




