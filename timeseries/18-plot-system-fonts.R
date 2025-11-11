## 18-plot-system-fonts.R
## Simple plot using a system font and ragg renderer; saves PNG to timeseries/figures.

library(ragg)
library(ggplot2)

fig_dir <- file.path("timeseries", "figures")
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)
outfile <- file.path(fig_dir, "fonts_demo.png")

grf <- ggplot(mtcars) +
  geom_point(aes(mpg, disp, colour = hp)) +
  labs(title = "System fonts — demo") +
  theme_minimal(base_family = "sans")

agg_png(outfile, width = 1000, height = 500, units = "px", scaling = 1)
print(grf)
dev.off()

cat(sprintf("Saved %s\n", outfile))

