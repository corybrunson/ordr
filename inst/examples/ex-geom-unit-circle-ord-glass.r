
# principal components analysis of glass composition measurements
glass[, c(5L, 7L, 8L, 10L, 11L)] %>%
  princomp(cor = TRUE) %>%
  as_tbl_ord() %>%
  cbind_rows(site = glass$Site, form = glass$Form) %>%
  augment_ord() %>%
  print() -> glass_pca
# note that column standard coordinates are unit vectors
rowSums(get_cols(glass_pca) ^ 2)
# plot column standard coordinates with a unit circle underlaid
glass_pca %>%
  ggbiplot(aes(label = name), sec.axes = "cols") +
  theme_biplot() +
  geom_rows_point(aes(color = site, shape = form), elements = "score") +
  geom_unit_circle(alpha = .5, scale.factor = 3) +
  geom_cols_vector()
