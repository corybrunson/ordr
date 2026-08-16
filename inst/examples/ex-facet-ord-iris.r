# scaled PCA of Anderson iris measurements
iris[, -5] %>%
  prcomp(scale = TRUE) %>%
  as_tbl_ord() %>%
  confer_inertia(1) %>%
  augment_ord() %>%
  mutate_rows(species = iris$Species) %>%
  print() -> iris_pca

# facet the first three principal components
# all four combinations of scales and space, from fixed to free
iris_pca %>%
  ggbiplot() +
  facet_ord(num_coord = 3) +
  geom_rows_point(aes(color = species)) +
  geom_cols_vector(aes(label = name), color = "#444444") +
  ggtitle(
    "Biplot of Anderson iris measurements",
    "Faceted pairs with fixed scales and fixed space"
  )

# free scales: each panel's scales are determined by its coordinates
iris_pca %>%
  ggbiplot() +
  facet_ord(num_coord = 3, scales = "free") +
  geom_rows_point(aes(color = species)) +
  geom_cols_vector(aes(label = name), color = "#444444") +
  ggtitle(
    "Biplot of Anderson iris measurements",
    "Faceted pairs with free scales and fixed space"
  )

# free scales and free space
iris_pca %>%
  ggbiplot() +
  facet_ord(num_coord = 3, scales = "free", space = "free") +
  geom_rows_point(aes(color = species)) +
  geom_cols_vector(aes(label = name), color = "#444444") +
  ggtitle(
    "Biplot of Anderson iris measurements",
    "Faceted pairs with free scales and free space"
  )
