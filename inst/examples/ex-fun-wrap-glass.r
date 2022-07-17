# glass composition data from one furnace
glass_banias <- subset(
  glass,
  Context == "L.15;B.166",
  select = c("SiO2", "Na2O", "CaO", "Al2O3", "MgO", "K2O")
)
# eigendecomposition of a covariance matrix
(glass_cov <- cov(glass_banias))
eigen_ord(glass_cov)
# singular value decomposition of a data matrix
svd_ord(glass_banias)
# classical multidimensional scaling of a distance matrix
cmdscale_ord(dist(glass_banias))
# canonical correlation analysis with trace components
glass_banias_minor <- subset(
  glass,
  Context == "L.15;B.166",
  select = c("TiO2", "FeO", "MnO", "P2O5", "Cl", "SO3")
)
# impute half of detection threshold
glass_banias_minor$TiO2[[1L]] <- 0.5
cancor_ord(glass_banias, glass_banias_minor)
