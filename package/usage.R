## ------------------------------------------------------------------
## Usage examples: CRAN downloads and country distribution
##
## This script shows two practical tasks:
##  1) Fetch and visualize CRAN download counts for a set of packages.
##  2) Inspect where downloads come from (by country) for one package
##     on a given date, then convert ISO codes to country names.
##
## The code is written to be easy to read and modify. Each block is
## clearly commented and small helper functions encapsulate steps.
## ------------------------------------------------------------------

## ---- setup: packages -------------------------------------------------
## Required packages. Install them beforehand if needed.
suppressPackageStartupMessages({
  library(ggplot2)
  library(dlstats)
  library(packageRank)
  library(dplyr)
  library(countrycode)
})

## ---- helpers ---------------------------------------------------------
#' Fetch CRAN download stats for multiple packages.
#'
#' @param pkgs Character vector of package names.
#' @return A tibble with daily downloads per package (dlstats::cran_stats).
fetch_cran_downloads <- function(pkgs) {
  dlstats::cran_stats(pkgs)
}

#' Plot downloads over time (log scale) by package.
#'
#' @param downloads Tibble from fetch_cran_downloads().
#' @return A ggplot object.
plot_downloads <- function(downloads) {
  ggplot(downloads, aes(x = end, y = downloads, group = package, color = package)) +
    geom_line() +
    geom_point(size = 1.2) +
    scale_y_log10(labels = scales::label_number_si()) +
    labs(
      title = "CRAN downloads over time",
      x = "Date",
      y = "Downloads (log scale)",
      color = "Package"
    ) +
    theme_minimal(base_size = 11)
}

#' Count package download requests by country for a specific date.
#'
#' @param pkg  Package name (single string).
#' @param date A date string in 'YYYY-MM-DD' format.
#' @return A tibble with columns: country (2-letter ISO), qtd (count).
count_downloads_by_country <- function(pkg, date) {
  logs <- packageRank::packageLog(packages = pkg, date = date)
  if (is.null(logs) || nrow(logs) == 0) return(dplyr::tibble(country = character(), qtd = integer()))
  logs |>
    dplyr::group_by(country) |>
    dplyr::summarise(qtd = dplyr::n(), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(qtd))
}

#' Convert ISO2 country codes to full country names.
#'
#' @param x Character vector of ISO2 country codes.
#' @return Character vector of country names.
iso2_to_name <- function(x) {
  countrycode::countrycode(x, origin = "iso2c", destination = "country.name")
}

## ---- 1) downloads for multiple packages ------------------------------
packages_to_check <- c("harbinger", "daltoolbox", "heimdall", "tspredit", "TSPred")
downloads <- fetch_cran_downloads(packages_to_check)

if (!is.null(downloads) && nrow(downloads) > 0) {
  # Peek at the first rows to understand the structure
  print(utils::head(downloads))

  # Build and print the plot (in interactive sessions this will display)
  p <- plot_downloads(downloads)
  print(p)
} else {
  message("No download data returned (check package names or network).")
}

## ---- 2) downloads by country for one day -----------------------------
target_pkg  <- "harbinger"
target_date <- "2025-01-15"  # change as needed (YYYY-MM-DD)

by_country <- count_downloads_by_country(target_pkg, target_date)
by_country <- dplyr::mutate(by_country, country = iso2_to_name(country))

# Display the result sorted by count
by_country


