---
title: "Classification iris"
date: "2026-03-23"
output:
  html_document:
    toc: true
    toc_depth: 2
---

Overview

This R Markdown document preserves the original example script and adds enough context to help you study, adapt, and rerun the workflow safely.

21-classification-iris.R Normalize Iris features, one-hot encode Species, and split data.

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
## 21-classification-iris.R
## Normalize Iris features, one-hot encode Species, and split data.

iris <- datasets::iris

norm <- minmax()
norm <- fit(norm, iris)
iris <- transform(norm, iris)

cm <- categ_mapping("Species")
iris_cm <- transform(cm, iris)

iris <- cbind(iris, iris_cm)
iris$Species <- NULL

colnames(iris) <- c("sepal_length", "sepal_width", "petal_length", "petal_width", "setosa", "versicolor", "virginica")

set.seed(1)
sr <- sample_random()
sr <- train_test(sr, iris)
iris_train <- sr$train
iris_test <- sr$test

dir.create("pytorch/data", showWarnings = FALSE, recursive = TRUE)
write.table(iris_train, file="pytorch/data/iris_train.csv", quote=FALSE, sep = ",", row.names = FALSE)
write.table(iris_test, file="pytorch/data/iris_test.csv", quote=FALSE, sep = ",", row.names = FALSE)
```




