Time Series Foundation Models (TSFM)
===================================

Overview
- Didactic experiments exploring tiny time series “foundation model” ideas, plus a baseline vanilla Transformer.
- Uses synthetic sine-wave data to keep the focus on modeling and workflow.

Reference
- Liang, Y., Wen, H., Nie, Y., Jiang, Y., Jin, M., Song, D., Pan, S., Wen, Q. Foundation Models for Time Series Analysis: A Tutorial and Survey. KDD 2024. DOI: 10.1145/3637528.3671451.

Related Work
- While these examples are intentionally minimal, they are inspired by the broader discussion of time series foundation models in the reference above.

Structure
- 00-run-example: `tsfm/00-run-example.R:1` — R launcher that calls the numbered Python scripts in order.
- 01 Pretrain: `tsfm/01-pretrain-foundation.py:1` — Masked reconstruction pretraining; saves `tiny_ts_model.pt`.
- 02 Fine-tune: `tsfm/02-finetune-forecasting.py:1` — Loads the checkpoint and trains a small forecasting head.
- 03 Visualize: `tsfm/03-visualize-embeddings.py:1` — Loads the checkpoint and visualizes embeddings with t-SNE.
- 10 Vanilla Transformer: `tsfm/10-vanilla-transformer.py:1` — Standalone encoder–decoder Transformer baseline.
- Shared core: `tsfm/tiny_ts_core.py:1` — Data generation, dataset wrapper, transformer encoder model, mask/step utils.

Data
- All examples use synthetic data generated on the fly; no external datasets are required.
- The pretrained weights are stored next to the scripts as `tsfm/tiny_ts_model.pt`.

How To Run
- Python-only: execute the numbered `.py` scripts directly.
- R-driven: run `tsfm/00-run-example.R:1` to call pretrain → finetune → visualize.

Notes
- Scripts include English comments and docstrings explaining each step.
- The numbered naming keeps workflow order clear and consistent across the repo.

Files
- [00-run-example.R](00-run-example.R) — R launcher to run pretrain → finetune → visualize.
- [01-pretrain-foundation.py](01-pretrain-foundation.py) — Masked reconstruction pretraining.
- [02-finetune-forecasting.py](02-finetune-forecasting.py) — Fine-tune forecasting head.
- [03-visualize-embeddings.py](03-visualize-embeddings.py) — t-SNE visualization of embeddings.
- [10-vanilla-transformer.py](10-vanilla-transformer.py) — Encoder–decoder Transformer baseline.
- [tiny_ts_core.py](tiny_ts_core.py) — Shared dataset/model utilities.
- [tiny_ts_model.pt](tiny_ts_model.pt) — Saved model checkpoint from pretraining.
