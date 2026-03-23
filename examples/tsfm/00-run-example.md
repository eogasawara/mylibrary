---
title: "Run example"
date: "2026-03-23"
output:
  html_document:
    toc: true
    toc_depth: 2
---

Overview

This R Markdown document preserves the original example script and adds enough context to help you study, adapt, and rerun the workflow safely.

00-run-example.R ----------------- Convenience R launcher for the tiny TS foundation model workflow. Uses relative paths so you can run it from the repo root.

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
## 00-run-example.R
## -----------------
## Convenience R launcher for the tiny TS foundation model workflow.
## Uses relative paths so you can run it from the repo root.

# Pretraining (builds and saves the foundation model)
reticulate::source_python("tsfm/01-pretrain-foundation.py")

# Fine-tuning (loads the saved model and adapts it to forecasting)
reticulate::source_python("tsfm/02-finetune-forecasting.py")

# Visualization (optional)
reticulate::source_python("tsfm/03-visualize-embeddings.py")
```




