PyTorch Experiments
===================

Overview
- This folder contains self-contained PyTorch experiments for tabular, time series, and graph data.
- Scripts are numbered semantically for easier navigation: data generation (01–04), classic ML tasks (20–22), time series models (30–32), graph-temporal models (40–41), and autoencoders (50–51).

Goals
- Provide minimal, didactic examples with clear comments in English.
- Standardize dataset paths under `pytorch/data/` and make scripts runnable from any working directory.

Structure
- Utilities: `pytorch/00-utils.py:1` — Seeding, model save/load, and DataFrame saver used by R generators.
- Data generators (R):
  - `pytorch/01-generate-classification-data.R:1` — 01-generate-classification-data (Iris), outputs `data_cla_{train,test}.csv`.
  - `pytorch/02-generate-regression-data.R:1` — 02-generate-regression-data (Boston), outputs `data_reg_{train,test}.csv`.
  - `pytorch/03-generate-timeseries-data.R:1` — 03-generate-timeseries-data (sine), outputs `data_ts_{train,test}.csv`.
  - `pytorch/04-generate-ts-classification-data.R:1` — 04-generate-ts-classification-data (events), outputs `data_tscla_{train,test}.csv`.
  - `pytorch/21-classification-iris.R:1` — Alternative Iris classification split, outputs `iris_{train,test}.csv`.
  - `pytorch/22-event-classification.R:1` — Event classification split, outputs `event_{train,test}.csv`.
- Experiments (Python):
  - `pytorch/20-regression-mlp.py:1` — MLP for tabular regression (reads `data_reg_*`).
  - `pytorch/30-ts-mlp.py:1` — MLP for time series windows (reads `data_ts_*`).
  - `pytorch/31-ts-lstm.py:1` — LSTM for time series windows (reads `data_ts_*`).
  - `pytorch/32-ts-conv1d.py:1` — 1D CNN for time series windows (reads `data_ts_*`).
  - `pytorch/40-gcn-lstm-forecasting.py:1` — Synthetic graph + LSTM forecasting.
  - `pytorch/41-gcn-anomaly-detection.py:1` — Synthetic GCN-based anomaly detection.
  - `pytorch/50-autoencoder.py:1` — Time series autoencoder helpers.
  - `pytorch/51-autoencoder-example.R:1` — R example calling the autoencoder via reticulate.

Data
- Location: `pytorch/data/` (see `pytorch/data/README.md:1` for filenames).
- Generators write here; Python scripts read from here using `Path(__file__).parent / "data"`.

How To Run
- R generators: run any `*_gen.R` or numbered R script; they create CSVs in `pytorch/data/`.
- Python experiments: run numbered `.py` scripts; they assume CSVs are in `pytorch/data/`.
- Dependencies:
  - Python: PyTorch (+ torch_geometric for GCN examples), NumPy, pandas.
  - R: daltoolbox, harbinger, reticulate.

Notes
- There are no Jupyter notebooks in this folder; no conversions were needed.
- All comments and messaging are in English for clarity.



Comparison: Graph Anomaly Approaches
- 41 GCN Anomaly Detection: Builds a fully connected graph over nodes and trains a GCN autoencoder to reconstruct inputs. Uses reconstruction MSE as anomaly score at the node/sample level.
- 42 Series2Graph Anomaly: Builds a k-NN similarity graph over sliding-window subsequences of a time series using cosine similarity. Trains a GCN encoder with a contrastive loss on consecutive windows and scores anomalies via embedding change magnitude between adjacent windows.
- 40 GCN+LSTM Forecasting: Focuses on graph-temporal forecasting, not anomaly detection. Combines GCN for per-node features with an LSTM over time, reporting prediction MSE, not anomaly scores.

Files
- [00-utils.py](00-utils.py) — Seeding, model save/load, and DataFrame saver.
- [01-generate-classification-data.R](01-generate-classification-data.R) — Generate Iris classification splits.
- [02-generate-regression-data.R](02-generate-regression-data.R) — Generate Boston regression splits.
- [03-generate-timeseries-data.R](03-generate-timeseries-data.R) — Generate time series windowed splits.
- [04-generate-ts-classification-data.R](04-generate-ts-classification-data.R) — Generate TS classification with events.
- [20-regression-mlp.py](20-regression-mlp.py) — Tabular regression MLP.
- [21-classification-iris.R](21-classification-iris.R) — Alternative Iris split writer.
- [22-event-classification.R](22-event-classification.R) — Event-labeled split writer.
- [30-ts-mlp.py](30-ts-mlp.py) — MLP for TS windows.
- [31-ts-lstm.py](31-ts-lstm.py) — LSTM for TS windows.
- [32-ts-conv1d.py](32-ts-conv1d.py) — 1D CNN for TS windows.
- [40-gcn-lstm-forecasting.py](40-gcn-lstm-forecasting.py) — Graph + LSTM forecasting.
- [41-gcn-anomaly-detection.py](41-gcn-anomaly-detection.py) — GCN anomaly detection.
- [42-series2graph-anomaly.py](42-series2graph-anomaly.py) — Series2Graph-style anomaly.
- [50-autoencoder.py](50-autoencoder.py) — Autoencoder helpers.
- [51-autoencoder-example.R](51-autoencoder-example.R) — R example for autoencoder.
- [data/README.md](data/README.md) — Data folder notes.
- [data/data_cla_train.csv](data/data_cla_train.csv), [data/data_cla_test.csv](data/data_cla_test.csv) — Iris classification data.
- [data/data_reg_train.csv](data/data_reg_train.csv), [data/data_reg_test.csv](data/data_reg_test.csv) — Boston regression data.
- [data/data_ts_train.csv](data/data_ts_train.csv), [data/data_ts_test.csv](data/data_ts_test.csv) — TS window data.
- [data/data_tscla_train.csv](data/data_tscla_train.csv), [data/data_tscla_test.csv](data/data_tscla_test.csv) — TS classification data.
- [data/event_train.csv](data/event_train.csv), [data/event_test.csv](data/event_test.csv) — Event classification splits.
