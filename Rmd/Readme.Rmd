Overview

This index lives under `Rmd/`, but the links below point to the rendered Markdown examples under `examples/`. Use the topic sections to navigate the generated material by subject.

Examples

- [Complete Index of Examples](examples/Readme.md)

## BigQuery

- [BigQuery Walkthrough](examples/bigquery/BigQuery.md) - Query BigQuery tables, join geographic data, and derive simple population-based estimates.

## Mobility

- [Merge and Save Mobility Data](examples/mobility/02-merge-and-save.md) - Combine partitioned mobility data into a single output artifact.

## PyTorch

- [Generate Classification Data](examples/pytorch/01-generate-classification-data.md) - Build a synthetic dataset for tabular classification experiments.
- [Generate Regression Data](examples/pytorch/02-generate-regression-data.md) - Build a synthetic dataset for regression experiments.
- [Generate Time Series Data](examples/pytorch/03-generate-timeseries-data.md) - Create univariate time series data for forecasting tests.
- [Generate Time Series Classification Data](examples/pytorch/04-generate-ts-classification-data.md) - Create labeled temporal sequences for classification tasks.
- [Iris Classification in R](examples/pytorch/21-classification-iris.md) - Train and inspect a simple classification workflow using the Iris dataset.
- [Event Classification](examples/pytorch/22-event-classification.md) - Prepare and model event-oriented data for supervised classification.
- [Autoencoder Example](examples/pytorch/51-autoencoder-example.md) - Train an autoencoder and inspect compressed representations or reconstruction behavior.

## R

- [CRAN Downloads and Country Distribution](examples/r/package-usage.md) - Analyze CRAN downloads over time and inspect country distribution for a selected day.

## References

- [Reference Utilities Overview](examples/references/Readme.md) - Overview of the bibliography and LaTeX utilities available in this repository.
- [Build Query Strings from BibTeX](examples/references/build_query_strings.md) - Generate boolean search expressions from DOI fields or normalized titles.
- [Check BibTeX File for Issues](examples/references/check_bib_errors.md) - Validate entries and report missing fields or common inconsistencies.
- [Clean a Single BibTeX File](examples/references/clean_bib_file.md) - Normalize one bibliography file before reuse.
- [Clean All BibTeX Files in a Directory](examples/references/clean_bibs_dir.md) - Apply the cleaning workflow to every `.bib` file in a directory tree.
- [Expand LaTeX Includes](examples/references/expand_tex_includes.md) - Resolve `\input{}`, `\include{}`, `\import{}`, and related directives into a single expanded file.
- [Export Google Scholar Publications](examples/references/export_scholar_publications.md) - Collect an author's publications and export the result to a spreadsheet.
- [Find and Remove Unused References in a Directory](examples/references/find_and_remove_unused_refs_dir.md) - Detect unused citation keys across multiple `.tex` files and prune the bibliography.
- [Find and Remove Unused References in a Single File](examples/references/find_and_remove_unused_refs_single.md) - Detect unused citation keys for one main LaTeX document and prune the bibliography.
- [Map and Replace Reference Keys in a Directory](examples/references/map_ref_keys_and_replace_dir.md) - Update BibTeX keys and propagate the replacements across a directory of LaTeX files.
- [Map and Replace Reference Keys in a Single File](examples/references/map_ref_keys_and_replace_single.md) - Update BibTeX keys and rewrite citations in one LaTeX document.
- [Merge BibTeX Files Pairwise](examples/references/merge_bibs_pairwise.md) - Reconcile multiple `.bib` files through pairwise merges.
- [Print DOI URLs](examples/references/print_doi_urls.md) - Emit `https://doi.org/...` links for entries that contain DOI values.
- [Union of BibTeX Files](examples/references/union_bibs.md) - Build a consolidated bibliography from a directory of source files.

## Reticulate

- [Invoke Python from R with Reticulate](examples/reticulate/02-invoke-reticulate.md) - Exchange data and function calls between R and Python.

## Statistics

- [Regression](examples/statistics/linear_regression.md) - Explore linear, polynomial, multiple, and logistic regression with plots and worked examples.
- [Effect Size Tutorial](examples/statistics/tut_effect_size.md) - Compare paired methods and compute effect size alongside nonparametric testing.
- [Nonparametric Tests Tutorial](examples/statistics/tut_nonparametric.md) - Perform normality checks and Wilcoxon-based comparisons for independent and paired samples.
- [Parametric Tests Tutorial](examples/statistics/tut_parametric.md) - Review t-test variants, assumptions, and interpretation for common experimental setups.

## Stocks

- [IBX50 Example](examples/stocks/ibx50.md) - Load and explore the IBX50 stock dataset with a small analysis workflow.

## Time Series

- [Reshape Data with Pivot Operations](examples/timeseries/01-data-reshape-pivot.md) - Transform time-indexed tables between wide and long formats.
- [Forecasting with ARIMA and Fourier Terms](examples/timeseries/02-forecasting-arima-fourier.md) - Fit a seasonal forecasting model using ARIMA components and Fourier features.
- [Kalman Filter for AR(1)](examples/timeseries/03-kalman-filter-ar1.md) - Implement and visualize a Kalman filtering workflow for an autoregressive process.
- [Event Detection with K-Means](examples/timeseries/04-event-detection-kmeans.md) - Cluster temporal behavior to identify event-like patterns.
- [Multivariate Anomaly Detection with PCA](examples/timeseries/05-multivariate-anomaly-pca.md) - Use principal components to monitor multivariate trajectories and flag anomalies.
- [Animated Detections](examples/timeseries/06-animated-detections.md) - Build animated visual summaries for evolving detections over time.
- [Maximum Likelihood for the Binomial Model](examples/timeseries/11-mle-binomial.md) - Derive and inspect likelihood and log-likelihood behavior for a binomial parameter.
- [Change Detection with EWMA](examples/timeseries/12-change-detection-ewma.md) - Use exponentially weighted moving averages to detect shifts in a process.
- [MLE for the Normal Mean](examples/timeseries/13-mle-normal-mean.md) - Estimate the mean parameter by maximizing the normal log-likelihood.
- [MLE for Normal Mean and Variance](examples/timeseries/14-mle-normal-params.md) - Estimate multiple normal parameters under a constrained optimization setup.
- [Density-Based Outlier Detection](examples/timeseries/15-density-outlier-detection.md) - Score observations with density estimation and highlight low-probability points.
- [Entropy and Surprisal for Anomaly Detection](examples/timeseries/16-entropy-surprisal-anomaly.md) - Convert empirical probabilities into surprisal-based anomaly scores.
- [Rolling Grubbs Test for Anomaly Detection](examples/timeseries/17-rolling-grubbs-anomaly.md) - Apply a rolling hypothesis test to identify local outliers.
- [Plot System Fonts](examples/timeseries/18-plot-system-fonts.md) - Enumerate and visualize fonts available in the local plotting environment.
- [Color Palette Demo](examples/timeseries/19-color-palette-demo.md) - Compare palette choices for charts and exploratory graphics.
- [Spline Example](examples/timeseries/21-spline.md) - Fit and visualize spline-based curves for smooth approximation.

## TSFM

- [Time Series Foundation Model Example](examples/tsfm/00-run-example.md) - Run a compact end-to-end example with a toy time series foundation model.

## Tutorial

- [Additional Graphics Examples](examples/tutorial/graphics_extra.md) - Small plotting demonstrations complementary to the statistics material.
- [Plot Drift Example](examples/tutorial/plot_drift.md) - Illustrate how distribution drift can be visualized over time.


