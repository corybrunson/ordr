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
