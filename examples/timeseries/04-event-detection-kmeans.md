---
title: "Event detection kmeans"
date: "2026-03-23"
output:
  html_document:
    toc: true
    toc_depth: 2
---

Overview

This R Markdown document preserves the original example script and adds enough context to help you study, adapt, and rerun the workflow safely.

04-event-detection-kmeans.R Univariate event detection using Harbinger's K-means method on example series.

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
## 04-event-detection-kmeans.R
## Univariate event detection using Harbinger's K-means method on example series.

library(harbinger)
library(dplyr)

# Load built-in example dataset
data(har_examples)

# Example A: time series 16
dataset <- har_examples[[16]]
plot(x = 1:length(dataset$serie), y = dataset$serie, main = "Series 16", xlab = "Time", ylab = "Value", type = "l")

model <- har_kmeans(k = 3)
model <- fit(model, dataset$serie)
detection <- detect(model, dataset$serie)
print(detection |> dplyr::filter(event == TRUE))

evaluation <- evaluate(model, detection$event, dataset$event)
print(evaluation$confMatrix)

grf <- plot.harbinger(model, dataset$serie, detection, dataset$event)
plot(grf)

# Example B: time series 1
dataset <- har_examples[[1]]
plot(x = 1:length(dataset$serie), y = dataset$serie, main = "Series 1", xlab = "Time", ylab = "Value", type = "l")

model <- har_kmeans(k = 1)
model <- fit(model, dataset$serie)
detection <- detect(model, dataset$serie)
print(detection |> dplyr::filter(event == TRUE))

evaluation <- evaluate(model, detection$event, dataset$event)
print(evaluation$confMatrix)

grf <- plot.harbinger(model, dataset$serie, detection, dataset$event)
plot(grf)
```




