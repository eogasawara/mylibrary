library(ggplot2)
library(dlstats)
library(packageRank)
library(dplyr)
library(countrycode)

x <- cran_stats(c("harbinger", "daltoolbox", "heimdall", "tspredit", "TSPred"))

if (!is.null(x)) {
  print(head(x))
  ggplot(x, aes(end, downloads, group=package, color=package)) +
    geom_line() + 
    geom_point() +
    scale_y_log10()
}

data <- packageLog(packages = "harbinger", date = '2025-01-15')
result <- data |> group_by(country) |> summarise(qtd = n()) |> arrange(desc(qtd))
result$country <- countrycode(result$country, origin = 'iso2c', destination = 'country.name')
result


