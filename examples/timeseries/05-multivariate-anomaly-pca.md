---
title: "Multivariate anomaly pca"
date: "2026-03-23"
output:
  html_document:
    toc: true
    toc_depth: 2
---

Overview

This R Markdown document preserves the original example script and adds enough context to help you study, adapt, and rerun the workflow safely.

05-multivariate-anomaly-pca.R Multivariate event detection using Harbinger (ARIMA baselines and PCA approach).

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
## 05-multivariate-anomaly-pca.R
## Multivariate event detection using Harbinger (ARIMA baselines and PCA approach).

library(harbinger)
library(dplyr)

# Load multivariate example dataset
data(har_examples_multi)

#Using the time series 9
dataset <- har_examples_multi[[1]]
head(dataset)

#ploting series x and y
plot(x = 1:length(dataset$x), y = dataset$x)
lines(x = 1:length(dataset$x), y = dataset$x)

plot(x = 1:length(dataset$y), y = dataset$y)
lines(x = 1:length(dataset$y), y = dataset$y)

# Analysis: variable x isolated
model <- har_arima()
model <- fit(model, dataset$x)
detection <- detect(model, dataset$x)
print(detection |> dplyr::filter(event==TRUE))
evaluation <- evaluate(model, detection$event, dataset$event)
print(evaluation$confMatrix)

# first variable isolated did not identify event
grf <- plot.harbinger(model, dataset$x, detection, dataset$event)
plot(grf)

# Analysis: variable y isolated
model <- har_arima()
model <- fit(model, dataset$y)
detection <- detect(model, dataset$y)
print(detection |> dplyr::filter(event==TRUE))
evaluation <- evaluate(model, detection$event, dataset$event)
print(evaluation$confMatrix)


# second variable isolated did not identify event
grf <- plot.harbinger(model, dataset$y, detection, dataset$event)
plot(grf)


# Multivariate: PCA method 
model <- har_multi_pca()

# fitting the model using the two columns of the dataset
model <- fit(model, dataset[,1:2])

# making detections using har_multi_pca
detection <- detect(model, dataset[,1:2])

# filtering detected events
print(detection |> dplyr::filter(event==TRUE))

evaluation <- evaluate(model, detection$event, dataset$event)
print(evaluation$confMatrix)


# Plot the results (extract the merged serie from detection)
serie <- attr(detection, "serie")
grf <- plot.harbinger(model, serie, detection, dataset$event)
plot(grf)
```




