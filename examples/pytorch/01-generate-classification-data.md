---
title: "Generate classification data"
date: "2026-03-23"
output:
  html_document:
    toc: true
    toc_depth: 2
---

Overview

This R Markdown document preserves the original example script and adds enough context to help you study, adapt, and rerun the workflow safely.

01-generate-classification-data.R Prepare normalized Iris data with one-hot targets and split.

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
## 01-generate-classification-data.R
## Prepare normalized Iris data with one-hot targets and split.

library(daltoolbox)

data(iris)
data <- iris

preproc <- minmax()
preproc <- fit(preproc, data)
data <- transform(preproc, data)

cm <- categ_mapping("Species")
data <- cbind(data, transform(cm, data))
data$Species <- NULL

sample <- sample_random()
tt <- train_test(sample, data)
train <- tt$train
test <- tt$test

library(reticulate)
source_python('pytorch/00-utils.py')

savedf(as.data.frame(train), 'pytorch/data/data_cla_train.csv')
savedf(as.data.frame(test), 'pytorch/data/data_cla_test.csv')
```




