# `seed.py`

Este modulo centraliza a configuracao de seeds para reproducibilidade em fluxos
Python e PyTorch.

## Funcao disponivel

`seed_everything(seed=1)`

- Converte o valor recebido para inteiro.
- Define o seed do modulo `random`.
- Define `PYTHONHASHSEED`.
- Define o seed do `numpy`.
- Define o seed do `torch`.
- Quando CUDA estiver disponivel, define o seed de todos os dispositivos.
- Ativa modo deterministico do cuDNN e desativa `benchmark`.

## Onde este codigo e chamado no repositorio

Os exemplos abaixo carregam `code/seed.py` via `reticulate::source_python(...)`
e chamam `seed_everything(1)`:

- `Rmd/chapter4/chap4_multivariate_autoencoder.Rmd`
- `Rmd/chapter4/chap4_multivariate_autoencoder3d.Rmd`
- `examples/chapter4/r/chap4_multivariate_autoencoder.R`
- `examples/chapter4/r/chap4_multivariate_autoencoder3d.R`

## Uso rapido em Python

```python
from seed import seed_everything

seed_everything(123)
```

## Uso rapido em R com `reticulate`

```r
library(reticulate)

source_python("code/seed.py")
set.seed(123)
seed_everything(123)
```
