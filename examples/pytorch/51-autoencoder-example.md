---
title: "Autoencoder example"
date: "2026-03-23"
output:
  html_document:
    toc: true
    toc_depth: 2
---

Overview

This R Markdown document preserves the original example script and adds enough context to help you study, adapt, and rerun the workflow safely.

51-autoencoder-example.R ------------------------ Demonstrates using the Python autoencoder helper on a synthetic time series with an injected anomaly.

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
##
## 51-autoencoder-example.R
## ------------------------
## Demonstrates using the Python autoencoder helper on a synthetic
## time series with an injected anomaly.

library(daltoolbox)

data(sin_data)

sin_data$y[39] <- sin_data$y[39]*6  # inject a spike anomaly

sw_size <- 5
ts <- ts_data(sin_data$y, sw_size)

preproc <- ts_norm_gminmax()
preproc <- fit(preproc, ts)
ts <- transform(preproc, ts)

samp <- ts_sample(ts, test_size = 10)
train <- as.data.frame(samp$train)
test <- as.data.frame(samp$test)

library(reticulate)
source_python('pytorch/50-autoencoder.py')

ae <- autoencoder_create(input_size = 5, encoding_size = 3)
ae <- autoencoder_fit(ae, train)

result_enc <- autoencoder_encode(ae, test)
print(head(result_enc))

result_recon <- autoencoder_encode_decode(ae, test)
print(head(result_recon, 10))
```




