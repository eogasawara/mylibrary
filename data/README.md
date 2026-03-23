Datasets Overview

This folder contains datasets organized by domain with clear names. Each entry links to the file and provides a brief description.

**Aviation**
- `aviation/airlines.csv` — US airline carrier codes and names.
- `aviation/aircraft_models.csv` — Aircraft model catalog with manufacturer.
- `aviation/airports_us.csv` — US airports with IATA code, city, state, coordinates.
- `aviation/airports_brazil.csv` — Brazilian airports with runway specs and coordinates.
- `aviation/routes_brazil.csv` — Route pairs (ICAO) for Brazilian airports (e.g., SBMH → SBBR).
- `aviation/us_flights_sample.csv` — Sample of US domestic flights (JFK-centric) with times and delays.
- `aviation/weather_jfk_2018.csv` — Daily weather observations associated to JFK (April 2018 sample).
- `aviation/weather_jfk_2018.RData` — R-serialized weather dataset matching the CSV.

**Classic ML**
- `classic_ml/iris.RData` — The classic Iris dataset in R format (species and flower measurements).
- `classic_ml/uci_wine.csv` — UCI Wine chemical analysis dataset (13 attributes, 3 classes).
- `classic_ml/uci_wine.RData` — R-serialized version of the UCI Wine dataset.

**WEKA Datasets**
- `weka/` — Collection of standard ARFF datasets for WEKA (e.g., `iris.arff`, `diabetes.arff`, `weather.nominal.arff`, Reuters samples). See filenames for specific topics and formats.

**Marketing**
- `marketing/customers.RData` — Sample customer records for analytics examples.
- `marketing/loyalty_clusters.csv` — Features for loyalty/frequent-flyer clustering (miles, flights, tenure).

**Graphics**
- `graphics/graphics_mobility.RData` — Mobility data for plotting examples.
- `graphics/graphics_map_reduce.RData` — Example dataset used in map–reduce visualization.
- `graphics/graphics_sin_cos.RData` — Generated sine and cosine series for charts.
- `graphics/graphics_monthly.RData` — Monthly aggregated series for graphics.
- `graphics/timetable.RData` — Timetable-shaped data for visualization.

**Time Series**
- `time-series/` — Time series collections:
  - Fertilizer consumption by country and nutrient: files named like `Brazil.N.RData`, `France.P2O5.RData`, `US.NPK.RData`, `Russia.K2O.RData` (units vary by source). Each file contains a univariate series for the specified country/nutrient.
  - Synthetic and market examples: `sin.RData`, `sin+x.RData`, `sin_x.RData`, `yahoo_sintetico.RData`, `yahoo_real3.RData`.
  - Water quality examples: `agua_ph.RData`, `agua_trueb.RData`.
  - Additional sample series: `3w_tipo5.RData`, `exemplo.RData`.

Notes
- Paths reflect the reorganized structure introduced in this README. If existing notebooks/scripts referenced old filenames, update those paths accordingly.
- If you need a dataset that was previously at the root (e.g., `wine.csv`), it has been moved into the domain folder listed above.
