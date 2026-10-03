# scaled PCA of Anderson iris measurements
iris[, -5] %>%
  prcomp(scale = TRUE) %>%
  as_tbl_ord() %>%
  confer_inertia(1) %>%
  augment_ord() %>%
  mutate_rows(species = iris$Species) %>%
  print() -> iris_pca

# facet the first three principal components
iris_pca %>%
  ggbiplot() +
  facet_ord(num_coord = 3) +
  geom_rows_point(aes(color = species)) +
  geom_cols_vector(aes(label = name), color = "#444444") +
  ggtitle(
    "Biplot of Anderson iris measurements",
    "Faceted pairs with fixed scales and space"
  )

# free space (by default frees the scales)
iris_pca %>%
  ggbiplot() +
  facet_ord(num_coord = 3, space = "free") +
  geom_rows_point(aes(color = species)) +
  geom_cols_vector(aes(label = name), color = "#444444") +
  ggtitle(
    "Biplot of Anderson iris measurements",
    "Faceted pairs with free scales and space"
  )

# free scales, fixed space: panel scales determined by coordinates (discouraged)
iris_pca %>%
  ggbiplot() +
  facet_ord(num_coord = 3, scales = "free", space = "fixed") +
  geom_rows_point(aes(color = species)) +
  geom_cols_vector(aes(label = name), color = "#444444") +
  ggtitle(
    "Biplot of Anderson iris measurements",
    "Faceted pairs with free scales and fixed space"
  )
