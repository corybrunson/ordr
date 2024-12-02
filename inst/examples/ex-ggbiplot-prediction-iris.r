# PCA of iris data
iris_pca <- ordinate(iris, cols = 1:4, prcomp, scale = TRUE)

# row-principal predictive biplot
iris_pca %>%
  augment_ord() %>%
  ggbiplot(axis.type = "predictive") +
  theme_bw() +# theme_biplot() +
  scale_color_brewer(type = "qual", palette = 2) +
  geom_cols_axis(aes(label = name, center = center, scale = scale)) +
  geom_rows_point(aes(color = Species), alpha = .5) +
  ggtitle("Predictive biplot of Anderson iris measurements")

# with two calibrated axes
iris_pca %>%
  augment_ord() %>%
  ggbiplot(axis.type = "predictive") +
  theme_bw() +# theme_biplot() +
  scale_color_brewer(type = "qual", palette = 2) +
  geom_origin() +
  # FIXME: Include this only for testing.
  geom_cols_axis(
    subset = c(2, 4),
    aes(label = name, center = center, scale = scale)
  ) +
  stat_cols_rule(
    .referent = "rows",
    subset = c(2, 4),
    aes(label = name, center = center, scale = scale)
  ) +
  geom_rows_point(aes(color = Species), alpha = .5) +
  expand_limits(x = c(-5, 5), y = c(-5, 5)) +
  ggtitle("Predictive biplot of Anderson iris measurements")
