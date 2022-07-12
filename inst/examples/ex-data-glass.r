head(glass)
glass_main <- subset(
  glass,
  Site == "Bet Eli'ezer",
  select = c("SiO2", "Na2O", "CaO", "Al2O3", "MgO", "K2O")
)
glass_main <- as.data.frame(glass_main)
rownames(glass_main) <- subset(glass, Site == "Bet Eli'ezer")$Anal
glass_lra <- lra(glass_main, compositional = TRUE, weighted = FALSE)
head(glass_lra$row.coords)
glass_lra$column.coords
glass_lra$sv
biplot(
  x = glass_lra$row.coords %*% diag(glass_lra$sv),
  y = glass_lra$column.coords,
  xlab = "Sample (principal coord.)",
  ylab = "Component (standard coord.)"
)
