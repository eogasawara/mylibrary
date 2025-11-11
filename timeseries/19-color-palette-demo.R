## 19-color-palette-demo.R
## Display a Brewer qualitative palette as color blocks.

library(RColorBrewer)

colors <- brewer.pal(9, "Set1")[ -6 ]
plot(NULL, xlim = c(0, length(colors)), ylim = c(0, 1), xlab = "", ylab = "", xaxt = "n", yaxt = "n")
rect(0:(length(colors) - 1), 0, 1:length(colors), 1, col = colors, border = NA)
title("RColorBrewer Set1 (excluding 6)")

