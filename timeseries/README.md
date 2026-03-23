Time Series Experiments
=======================

Overview
- Didactic R experiments covering forecasting, Kalman filtering, event detection (Harbinger), multivariate anomaly detection, data reshaping, and animated visualization. Scripts are renamed with semantic numbering and include English comments.

Data
- Local datasets are expected under `timeseries/data/` (see `timeseries/data/README.md:1`). Scripts resolve paths using `file.path("timeseries", "data", ...)` so they can be run from the repo root.

Experiments (semantic order)
- [01-data-reshape-pivot.R](01-data-reshape-pivot.R) — Join yearly temps with oil metrics and pivot monthly to wide.
- [02-forecasting-arima-fourier.R](02-forecasting-arima-fourier.R) — ARIMA vs ARIMA+Fourier on monthly temps.
- [03-kalman-filter-ar1.R](03-kalman-filter-ar1.R) — Kalman filter on AR(1) fitted to yearly temps.
- [04-event-detection-kmeans.R](04-event-detection-kmeans.R) — Harbinger K-means event detection on example series.
- [05-multivariate-anomaly-pca.R](05-multivariate-anomaly-pca.R) — Harbinger PCA-based multivariate detection.
- [06-animated-detections.R](06-animated-detections.R) — Animate detections over time and save GIF/PNG.

Other scripts
- [21-spline.R](21-spline.R) — Spline smoothing example.
- [11-mle-binomial.R](11-mle-binomial.R) — Binomial MLE for coin-flip probability.
- [12-change-detection-ewma.R](12-change-detection-ewma.R) — EWMA-based change detection.
- [13-mle-normal-mean.R](13-mle-normal-mean.R) — MLE of Normal mean with known variance.
- [14-mle-normal-params.R](14-mle-normal-params.R) — MLE of Normal mean and sigma.
- [15-density-outlier-detection.R](15-density-outlier-detection.R) — KDE-based outlier detection.
- [16-entropy-surprisal-anomaly.R](16-entropy-surprisal-anomaly.R) — Surprise score anomaly demo.
- [17-rolling-grubbs-anomaly.R](17-rolling-grubbs-anomaly.R) — Rolling-window Grubbs' test for outliers.
- [18-plot-system-fonts.R](18-plot-system-fonts.R) — Plot rendered with ragg and system font.
- [19-color-palette-demo.R](19-color-palette-demo.R) — RColorBrewer palette visualization.

