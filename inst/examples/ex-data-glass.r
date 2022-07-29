# subset glass data to one site and major components
head(glass)
glass_main <- subset(
  glass,
  Site == "Bet Eli'ezer",
  select = c("SiO2", "Na2O", "CaO", "Al2O3", "MgO", "K2O")
)
# format as a data frame with row names
glass_main <- as.data.frame(glass_main)
rownames(glass_main) <- subset(glass, Site == "Bet Eli'ezer")$Anal

# perform log-ratio analysis
glass_lra <- lra(glass_main, compositional = TRUE, weighted = FALSE)
# inspect LRA row and column coordinates
head(glass_lra$row.coords)
glass_lra$column.coords
# inspect singular values of LRA
glass_lra$sv

# plot samples and measurements in a biplot
biplot(
  x = glass_lra$row.coords %*% diag(glass_lra$sv),
  y = glass_lra$column.coords,
  xlab = "Sample (principal coord.)", ylab = ""
)
mtext("Component (standard coord.)", side = 4L, line = 3L)
