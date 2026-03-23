PyTorch Data Folder
===================

Purpose
- Centralizes CSV datasets used by the PyTorch experiments in this folder.
- Scripts prefer reading from here using paths computed relative to their own file.

Expected files (produced by the R generators)
- data_cla_train.csv, data_cla_test.csv — normalized Iris classification splits
- data_reg_train.csv, data_reg_test.csv — normalized Boston housing regression splits
- data_ts_train.csv, data_ts_test.csv — sliding-window sine time series splits
- data_tscla_train.csv, data_tscla_test.csv — time series classification splits with events
- iris_train.csv, iris_test.csv — alternative Iris splits (from 21-classification-iris.R)
- event_train.csv, event_test.csv — event classification splits (from 22-event-classification.R)

Notes
- Generators: see files under `pytorch` named starting with `01-` through `04-` and `21-`, `22-`.
- Consumers: Python experiments look for these files using a path like `Path(__file__).parent / "data"`.
