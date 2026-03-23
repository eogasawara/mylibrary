---
title: "Spline"
date: "2026-03-23"
output:
  html_document:
    toc: true
    toc_depth: 2
---

Overview

This R Markdown document preserves the original example script and adds enough context to help you study, adapt, and rerun the workflow safely.

This document wraps the original script '21-spline.R' in R Markdown so the code can be read and executed with contextual guidance.

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
df <- data.frame(x=1:20,
                 y=c(2, 4, 7, 9, 13, 15, 19, 16, 13, 10,
                     11, 14, 15, 15, 16, 15, 17, 19, 18, 20))

#view head of data frame
head(df)

#create scatterplot
plot(df$x, df$y, cex=1.5, pch=19)

#fit simple linear regression model
linear_fit <- lm(df$y ~ df$x)

#view model summary
summary(linear_fit)

#create scatterplot
plot(df$x, df$y, pch=19)
lines(df$x, linear_fit$fitted.values)

#add regression line to scatterplot
abline(linear_fit)

library(splines)

#fit spline regression model
spline_fit <- lm(df$y ~ bs(df$x))

#view summary of spline regression model
summary(spline_fit)

#create scatterplot
plot(df$x, df$y, pch=19)
lines(df$x, spline_fit$fitted.values)
```




