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
