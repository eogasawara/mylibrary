library(ggplot2)
library(dlstats)
library(packageRank)
library(dplyr)
library(countrycode)

x_full <- cran_stats(c("harbinger", "daltoolbox", "daltoolboxdp", "heimdall", "tspredit", "TSPred"))



data <- packageLog(packages = c("harbinger", "daltoolbox", "daltoolboxdp", "heimdall", "tspredit", "TSPred"), date = '2026-03-21')

result <- data$harbinger |> group_by(country) |> summarise(qtd = n()) |> arrange(desc(qtd))
result$country <- countrycode(result$country, origin = 'iso2c', destination = 'country.name')
result


if (!is.null(x)) {
  x <- x_full |> filter(start >= as.Date("2023-09-01"))
  print(head(x))
  ggplot(x, aes(end, downloads, group=package, color=package)) +
    geom_line() + 
    geom_point() +
    scale_y_log10()
}