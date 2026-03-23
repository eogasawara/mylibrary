---
title: "Mle normal mean"
date: "2026-03-23"
output:
  html_document:
    toc: true
    toc_depth: 2
---

Overview

This R Markdown document preserves the original example script and adds enough context to help you study, adapt, and rerun the workflow safely.

13-mle-normal-mean.R MLE of the mean (mu) for a Normal distribution assuming known variance.

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
## 13-mle-normal-mean.R
## MLE of the mean (mu) for a Normal distribution assuming known variance.

set.seed(1)
scores <- c(70, 75, 85, 90, 78)
sigma2 <- var(scores)  # treat as known for demo

loglik <- function(mu) {
  n <- length(scores)
  -n/2 * log(2 * pi * sigma2) - sum((scores - mu)^2) / (2 * sigma2)
}

# Maximize log-likelihood over a reasonable interval
opt <- optimize(loglik, interval = range(scores), maximum = TRUE)
mle_mu <- opt$maximum

cat(sprintf("MLE of mu: %.4f\n", mle_mu))
```




