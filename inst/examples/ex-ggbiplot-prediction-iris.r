# PCA of iris data
iris_pca <- ordinate(iris, cols = 1:4, prcomp, scale = TRUE)

# row-principal predictive biplot
iris_pca %>%
  augment_ord() %>%
  ggbiplot(axis.type = "predictive") +
  theme_bw() +
  scale_color_brewer(type = "qual", palette = 2) +
  geom_cols_axis(aes(label = name, center = center, scale = scale)) +
  geom_rows_point(aes(color = Species), alpha = .5) +
  ggtitle("Predictive biplot of Anderson iris measurements")

# with only one calibrated axis
iris_pca %>%
  augment_ord() %>%
  ggbiplot(axis.type = "predictive") +
  theme_bw() +
  scale_color_brewer(type = "qual", palette = 2) +
  geom_cols_vector() +
  geom_cols_axis(
    # TODO: allow a character vector to be passed to `subset`
    subset = 2L,
    aes(label = name, center = center, scale = scale),
    # TODO: limit the axis to values taken by data elements
    # (allow parameter to also accept two extremum values)
    axis_limits = "range",
    # TODO: offset until a sufficient margin between data points is available
    # (allow parameter to also accept an orthogonal distance from the origin)
    offset_margin = 0.3
  ) +
  geom_rows_point(aes(color = Species), alpha = .5) +
  ggtitle("Predictive biplot of Anderson iris measurements")
