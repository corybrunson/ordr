# Scaled PCA of Anderson iris data with annotated biplot
iris[, -5] %>%
  prcomp(scale = TRUE) %>%
  as_tbl_ord() %>%
  confer_inertia(1) %>%
  mutate_rows(species = iris$Species) %>%
  print() -> iris_pca
iris_pca %>%
  ggbiplot(aes(color = species)) +
  theme_bw() +
  scale_color_brewer(type = "qual", palette = 2) +
  geom_rows_point(alpha = .5) +
  stat_rows_center(fun.center = "mean", size = 3, shape = "triangle") +
  stat_rows_ellipse(level = .99) +
  geom_cols_vector(color = "#444444") +
  ggtitle(
    "Row-principal PCA biplot of Anderson iris data",
    "Overlaid with centroids and 99% confidence ellipses"
  )
iris_pca %>%
  ggbiplot(aes(color = species)) +
  theme_bw() +
  geom_rows_point() +
  stat_rows_center(fun.center = "mean", size = 3, shape = 1) +
  geom_polygon(
    aes(fill = species),
    color = NA, alpha = .25, stat = "rows_ellipse"
  ) +
  geom_cols_vector(color = "#444444") +
  scale_color_brewer(
    type = "qual", palette = 2,
    aesthetics = c("color", "fill")
  ) +
  ggtitle(
    "Row-principal PCA biplot of Anderson iris data",
    "Overlaid with 99% confidence disks"
  )
