---
title: "Event classification"
date: "2026-03-23"
output:
  html_document:
    toc: true
    toc_depth: 2
---

Overview

This R Markdown document preserves the original example script and adds enough context to help you study, adapt, and rerun the workflow safely.

22-event-classification.R Build a sliding-window dataset with event labels from Harbinger examples.

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
## 22-event-classification.R
## Build a sliding-window dataset with event labels from Harbinger examples.

source("https://raw.githubusercontent.com/cefet-rj-dal/harbinger-examples/main/jupyter.R")

# loading Harbinger
load_library("daltoolbox") 
load_library("harbinger")

# loading the example database
data(har_examples)

# Using the time series 17 
dataset <- harbinger::har_examples$example17

y <- dataset$serie
event <- as.integer(dataset$event)
data <- ts_data(y, 5)
data <- as.data.frame(data)

norm <- minmax()
norm <- fit(norm, data)
data <- transform(norm, data)

data$event <- event[5:length(event)]

train <- data[1:80,]
test <- data[81:109,]

dir.create("pytorch/data", showWarnings = FALSE, recursive = TRUE)
write.table(train, file="pytorch/data/event_train.csv", quote=FALSE, sep = ",", row.names = FALSE)
write.table(test, file="pytorch/data/event_test.csv", quote=FALSE, sep = ",", row.names = FALSE)
```




