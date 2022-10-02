# compute unscaled row-principal components of scaled measurements
(iris_pca <- ordinate(iris, cols = 1:4, princomp))

# row-principal biplot with coordinate-wise standard deviations
iris_pca %>%
  ggbiplot(aes(color = Species)) +
  theme_bw() +
  scale_color_brewer(type = "qual", palette = 2) +
  geom_unit_circle() +
  geom_rows_point(alpha = .5) +
  geom_cols_vector(color = "#444444") +
  geom_cols_text_radiate(aes(label = name), color = "#444444") +
  ggtitle("Row-principal unscaled PCA biplot of Anderson iris measurements") +
  expand_limits(y = c(NA, 2))
